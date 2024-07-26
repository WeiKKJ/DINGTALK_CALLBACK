class ZCL_DINGTALK_CALLBACK definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  class-methods ADD_LOG
    importing
      value(NAME) type RS38L_FNAM
      value(EVENTTYPE) type RS38L_PAR_
      value(DETAIL_ORI) type ANY
      value(DETAIL) type ANY optional
      value(SECDS) type ZILOGSECDS .
protected section.
private section.

  data MY_PARAMS type TIHTTPNVP .

  methods GET_PARAMS
    importing
      !PARAMS type STRING
    returning
      value(MY_PARAMS) type TIHTTPNVP .
ENDCLASS.



CLASS ZCL_DINGTALK_CALLBACK IMPLEMENTATION.


  METHOD get_params.
    DATA:lt_kv_tab TYPE TABLE OF string,
         wa_kv     TYPE ihttpnvp.
    CLEAR:lt_kv_tab,wa_kv,my_params.
    SPLIT params AT '&' INTO TABLE lt_kv_tab.
    LOOP AT lt_kv_tab ASSIGNING FIELD-SYMBOL(<lt_kv_tab>).
      CLEAR wa_kv.
      SPLIT <lt_kv_tab> AT '=' INTO wa_kv-name wa_kv-value.
      APPEND wa_kv TO my_params.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    DATA:lt_header TYPE tihttpnvp,
         json      TYPE string,
         proto     TYPE string,
         host      TYPE string,
         port      TYPE string.
    TYPES:BEGIN OF t_JSON1,
            msg_signature TYPE string,
            timestamp     TYPE string,
            nonce         TYPE string,
            encrypt       TYPE string,
          END OF t_JSON1.
    DATA:wa_encrypt TYPE t_JSON1.
    DATA:text TYPE string.
    DATA:dingCryptode TYPE REF TO zcl_dingtalk_callback_crypto.
    DATA: zilogt1 TYPE i,
          zilogt2 TYPE i,
          secds   TYPE zilogsecds.
    " 解密后的消息体结构  02.05.2024 18:23:28 by kkw
    TYPES: BEGIN OF t_JSON1_event,
             eventtype TYPE string,
           END OF t_JSON1_event.
    DATA:wa_event TYPE t_JSON1_event.
*返回消息
    DEFINE http_msg.
      server->response->set_header_field( name = 'Content-Type' value = 'application/json;charset=utf-8' ).
      server->response->set_status( code = 200  reason = 'ok' ).
      server->response->set_cdata( EXPORTING data   = &1 ).
    END-OF-DEFINITION.

    CLEAR:lt_header,json.
    server->request->get_header_fields( CHANGING fields = lt_header ).

    READ TABLE lt_header INTO DATA(wa_header) WITH KEY name = '~request_method' .
    CASE wa_header-value.
      WHEN 'GET'.
        DATA(msg) = `{"rtype": "S","rtmsg": "钉钉回调服务已启动",`
        && `"author":"kkw","mail":"weikj@foxmail.com"}`.
        http_msg msg.
      WHEN 'POST'.
        GET RUN TIME FIELD zilogt1.
*获取query参数,必须包含appid参数
        CLEAR:my_params,json.
        READ TABLE lt_header INTO DATA(wa_params) WITH KEY name = '~query_string' .
        my_params = me->get_params( EXPORTING params = wa_params-value ).
        json = server->request->if_http_entity~get_cdata( ).

        READ TABLE my_params ASSIGNING FIELD-SYMBOL(<my_params>) WITH KEY name = 'appid'.
        IF sy-subrc EQ 0.
*从配置表获取加密 aes_key、签名 token以及AppKey
          SELECT SINGLE * FROM ztddconfig WHERE appid = @<my_params>-value INTO @DATA(wa_ztddconf).
          IF sy-subrc NE 0.
            http_msg `appid config not found`.
            RETURN.
          ENDIF.
        ELSE.
          http_msg `appid params missing`.
          RETURN.
        ENDIF.
        " 添加@机器人的回调处理,暂不处理回复逻辑  13.06.2024 14:33:01 by kkw
        READ TABLE my_params ASSIGNING <my_params> WITH KEY name = 'robot'.
        IF sy-subrc EQ 0.
          CALL METHOD me->add_log
            EXPORTING
              name       = CONV rs38l_fnam( 'ZDT_CB_ROBOT' )
              eventtype  = CONV rs38l_par_( |{ wa_ztddconf-name }&{ <my_params>-value }| ) "robot name
              detail_ori = json
*             detail     = text
              secds      = secds.
          http_msg `Welcome to ABAP`.
          RETURN.
        ENDIF.
        "事件订阅的回调处理解密处理
        CLEAR:wa_encrypt.
        /ui2/cl_json=>deserialize( EXPORTING json = json  pretty_name = /ui2/cl_json=>pretty_mode-low_case CHANGING data = wa_encrypt ).

        READ TABLE my_params ASSIGNING <my_params> WITH KEY name = 'signature'.
        IF sy-subrc EQ 0.
          wa_encrypt-msg_signature = <my_params>-value.
        ENDIF.
        READ TABLE my_params ASSIGNING <my_params> WITH KEY name = 'timestamp'.
        IF sy-subrc EQ 0.
          wa_encrypt-timestamp = <my_params>-value.
        ENDIF.
        READ TABLE my_params ASSIGNING <my_params> WITH KEY name = 'nonce'.
        IF sy-subrc EQ 0.
          wa_encrypt-nonce = <my_params>-value.
        ENDIF.
        DATA(json_ori) = /ui2/cl_json=>serialize( data = wa_encrypt  compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
*解密请求报文
        FREE dingcryptode.
        CREATE OBJECT dingcryptode
          EXPORTING
            token          = wa_ztddconf-cbtoken
            encodingaeskey = wa_ztddconf-cbencodingAesKey
            key            = wa_ztddconf-appkey.
        CLEAR:text.
        CALL METHOD dingcryptode->getdecryptmsg
          EXPORTING
            msg_signature         = wa_encrypt-msg_signature
            timestamp             = wa_encrypt-timestamp
            nonce                 = wa_encrypt-nonce
            content               = wa_encrypt-encrypt
          RECEIVING
            text                  = text
          EXCEPTIONS
            signature_check_error = 1
            contentx_error        = 2
            padding_error         = 3
            appkey_error          = 4
            OTHERS                = 5.
        GET RUN TIME FIELD zilogt2.
        secds = ( zilogt2 - zilogt1 ) / 1000000.
        IF sy-subrc <> 0.
          CALL METHOD me->add_log
            EXPORTING
              name       = CONV rs38l_fnam( 'ZDT_CB' )
              eventtype  = CONV rs38l_par_( wa_event-eventtype )
              detail_ori = json_ori
              detail     = `{"msg":"解密钉钉回调请求报文失败","cbtoken":"` && wa_ztddconf-cbtoken && `","cbencodingAesKey":"` && wa_ztddconf-cbencodingAesKey
                           && `","cbencodingAesKey":"` && wa_ztddconf-appkey
                           && `"}`
              secds      = secds.

*   Implement suitable error handling here
          http_msg `error`.
          RETURN.
        ENDIF.
        CLEAR wa_event.
        /ui2/cl_json=>deserialize( EXPORTING json = text  pretty_name = /ui2/cl_json=>pretty_mode-low_case CHANGING data = wa_event ).
        CALL METHOD me->add_log
          EXPORTING
            name       = CONV rs38l_fnam( 'ZDT_CB' )
            eventtype  = CONV rs38l_par_( wa_event-eventtype )
            detail_ori = json_ori
            detail     = text
            secds      = secds.

        CASE wa_event-eventtype.
          WHEN 'check_url'." 验证请求  02.05.2024 23:28:48 by kkw
            CALL METHOD dingcryptode->getencryptedmap
              EXPORTING
                content = `success`
              RECEIVING
                res     = DATA(res).
            http_msg res.
          WHEN OTHERS.
            CALL METHOD dingcryptode->getencryptedmap
              EXPORTING
                content = `success`
              RECEIVING
                res     = res.
            http_msg res.
        ENDCASE.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD add_log.
    DATA:BEGIN OF zilogkeystr,
           name   TYPE zabap_log-name,
           erdat  TYPE zabap_log-erdat,
           stamp  TYPE zabap_log-stamp,
           indx   TYPE zabap_log-indx,
           fdname TYPE zabap_log-fdname,
         END OF zilogkeystr.
    DATA:wa_zilogdata TYPE zabap_log.
    DATA:zilogzonlo     TYPE sy-zonlo,
         zilogtsl       TYPE timestampl,
         zilogtsstr(30).
    DATA:loginusers    TYPE TABLE OF uinfo,
         loginusers_wa TYPE uinfo,
         curusertid    TYPE          sy-index.

    GET TIME STAMP FIELD zilogtsl.
    zilogkeystr-name = name.
    IF sy-zonlo IS INITIAL.
      zilogzonlo = 'UTC+8'.
    ELSE.
      zilogzonlo = sy-zonlo.
    ENDIF.
    WRITE zilogtsl TIME ZONE zilogzonlo TO zilogtsstr .
    zilogkeystr-erdat = sy-datum.
    zilogkeystr-stamp = zilogtsstr+11(15).
    CALL FUNCTION 'THUSRINFO'
      TABLES
        usr_tabl = loginusers.
    CALL FUNCTION 'TH_USER_INFO'
      IMPORTING
        tid = curusertid.
    READ TABLE loginusers INTO loginusers_wa WITH KEY tid = curusertid.

    zilogkeystr-indx   = '10'.
    zilogkeystr-fdname = eventtype.

    wa_zilogdata-area  = sy-repid+4.
    wa_zilogdata-ernam = sy-uname.
    wa_zilogdata-memo  = 'B'.
    wa_zilogdata-erdat = sy-datum.
    wa_zilogdata-uterm = loginusers_wa-term.
    wa_zilogdata-secds = 0.
    wa_zilogdata-fdname = eventtype.
    EXPORT detail FROM detail_ori TO DATABASE zabap_log(fl) ID zilogkeystr FROM wa_zilogdata.

    IF detail IS NOT INITIAL.
      zilogkeystr-indx   = '20'.
      wa_zilogdata-memo  = 'R'.
      wa_zilogdata-secds = secds.
      EXPORT detail FROM detail TO DATABASE zabap_log(fl) ID zilogkeystr FROM wa_zilogdata.
    ENDIF.
    COMMIT WORK.

*DEFINE zfmdatasave1.
*  DATA: header_gd TYPE header_fb,
*        tables_gd TYPE rsfb_para WITH HEADER LINE,
*        import_gd TYPE rsfb_para WITH HEADER LINE,
*        export_gd TYPE rsfb_para WITH HEADER LINE,
*        change_gd TYPE rsfb_para WITH HEADER LINE,
*        pname_gd  TYPE tfdir-pname.
*  DATA: BEGIN OF zilogkeystr,
*          name   LIKE zfmdata-name,
*          erdat  LIKE zfmdata-erdat,
*          stamp  LIKE zfmdata-stamp,
*          indx   LIKE zfmdata-indx,
*          fdname LIKE zfmdata-fdname,
*        END OF zilogkeystr.
*  DATA: wa_zilogdata       TYPE zfmdata,
*        wa_zfmdatacfg      TYPE zfmdatacfg,
*        zilogtsl           TYPE timestampl,
*        zilogtsstr(30),
*        zilogindx          TYPE numc1,
*        zilogfsstr         TYPE string,
*        zilogstopallrecord , "不再记录所有的函数LOG
*        zilogrecordnodata,   "只记录调用历史，不记录具体数据
*        zilogrecordfmstop .  "不记录本函数LOG
*  DATA: zilogt1    TYPE i,
*        zilogt2    TYPE i,
*        zilogzonlo TYPE sy-zonlo.
*  DATA: loginusers TYPE TABLE OF uinfo WITH HEADER LINE,
*        curusertid TYPE          sy-index.
*  DATA: lt_zilogfmstack TYPE TABLE OF sys_calls WITH HEADER LINE .
*  FIELD-SYMBOLS: <fs_zrfclog> TYPE any .
*
*  header_gd-name = &1.
*  CALL FUNCTION 'SYSTEM_CALLSTACK'
*    IMPORTING
*      et_callstack = lt_zilogfmstack[].
*  READ TABLE lt_zilogfmstack INDEX 1.
*  header_gd-name = lt_zilogfmstack-eventname.
*
*  SELECT SINGLE * INTO wa_zfmdatacfg
*    FROM zfmdatacfg
*    BYPASSING BUFFER
*    WHERE fname = 'STOPALLFMRECORD'.
*  IF sy-subrc = 0.
*    zilogstopallrecord = 'X'.
*  ENDIF.
*
*  SELECT SINGLE * INTO wa_zfmdatacfg FROM zfmdatacfg
*    WHERE fname = header_gd-name.
*  IF wa_zfmdatacfg-exitfm = 'X' AND zilogstopallrecord = ''.
*    RETURN.
*  ENDIF.
*  IF wa_zfmdatacfg-brkuser = sy-uname AND zilogstopallrecord = ''.
*    sy-subrc = 4.
*    sy-fmkey = ''.
*    WHILE sy-subrc = 4 AND sy-fmkey = ''.
*      SELECT SINGLE * INTO wa_zfmdatacfg FROM zfmdatacfg
*        WHERE fname = header_gd-name AND
*              brkuser <> sy-uname.
*    ENDWHILE.
*  ENDIF.
*  IF wa_zfmdatacfg-nrindex = 'H'.
*    zilogrecordnodata = 'X'.
*  ENDIF.
*  IF wa_zfmdatacfg-nrindex = 'N'.
*    zilogrecordfmstop = 'X'.
*  ENDIF.
*
*  IF zilogstopallrecord = '' AND zilogrecordfmstop = ''
*                             AND zilogrecordnodata = ''.
*    SELECT SINGLE pname INTO pname_gd FROM tfdir
*      WHERE funcname =  header_gd-name.
*    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
*      EXPORTING
*        program       = pname_gd
*      IMPORTING
*        group         = header_gd-area
*        namespace     = header_gd-namespace
*      EXCEPTIONS
*        error_message = 1
*        othe          = 12.
*    IF sy-subrc = 0.
*      CONCATENATE header_gd-namespace header_gd-area
*                    INTO header_gd-area.
*      CALL METHOD cl_fb_parameter_db=>read
*        IMPORTING
*          tables = tables_gd[]
*          import = import_gd[]
*          export = export_gd[]
*          change = change_gd[]
*        CHANGING
*          header = header_gd.
*    ENDIF.
*  ENDIF.
*
*  IF zilogstopallrecord = '' AND zilogrecordfmstop = ''.
*    GET RUN TIME FIELD zilogt1.
*    GET TIME STAMP FIELD zilogtsl.
*    zilogkeystr-name = header_gd-name.
*    IF sy-zonlo IS INITIAL.
*      zilogzonlo = 'UTC+8'.
*    ELSE.
*      zilogzonlo = sy-zonlo.
*    ENDIF.
*    WRITE zilogtsl TIME ZONE zilogzonlo TO zilogtsstr .
*    zilogkeystr-erdat = sy-datum.
*    zilogkeystr-stamp = zilogtsstr+11(15).
*
*    CALL FUNCTION 'THUSRINFO'
*      TABLES
*        usr_tabl = loginusers.
*    CALL FUNCTION 'TH_USER_INFO'
*      IMPORTING
*        tid = curusertid.
*    READ TABLE loginusers WITH KEY tid = curusertid.
*  ENDIF.
*END-OF-DEFINITION.
*
*DEFINE zfmdatasave2.
*  IF zilogstopallrecord = '' AND zilogrecordfmstop = ''.
*    GET RUN TIME FIELD zilogt2.
*    zilogindx = zilogindx + 1 .
*    IF zilogindx < 10 AND zilogindx NA wa_zfmdatacfg-nrindex AND
*       ( zilogrecordnodata = '' OR zilogrecordnodata = 'X' AND zilogindx = 1 ) .
*      zilogkeystr-indx   = zilogindx.
*      wa_zilogdata-area  = sy-repid+4.
*      wa_zilogdata-ernam = sy-uname.
*      wa_zilogdata-memo  = &1 .
*      wa_zilogdata-erdat = sy-datum.
*      wa_zilogdata-uterm = loginusers-term .
*      wa_zilogdata-secds = ( zilogt2 - zilogt1 ) / 1000000 .
*
*      IF wa_zfmdatacfg-rtypemp <> ''.
*        ASSIGN (wa_zfmdatacfg-rtypemp) TO <fs_zrfclog>.
*      ELSE.
*        ASSIGN ('RTYPE') TO <fs_zrfclog>.
*      ENDIF.
*      IF sy-subrc = 0.
*        wa_zilogdata-rtype = <fs_zrfclog>.
*      ENDIF.
*
*      IF wa_zfmdatacfg-rtmsgmp <> ''.
*        ASSIGN (wa_zfmdatacfg-rtmsgmp) TO <fs_zrfclog>.
*      ELSE.
*        ASSIGN ('RTMSG') TO <fs_zrfclog>.
*      ENDIF.
*      IF sy-subrc = 0.
*        wa_zilogdata-rtmsg = <fs_zrfclog>.
*      ENDIF.
*
*      LOOP AT import_gd.
*        ASSIGN (import_gd-parameter) TO <fs_zrfclog>.
*        CHECK sy-subrc = 0 .
*        zilogkeystr-fdname = import_gd-parameter.
*        EXPORT <fs_zrfclog> TO DATABASE zfmdata(fl) ID zilogkeystr FROM wa_zilogdata.
*      ENDLOOP.
*
*      LOOP AT change_gd.
*        ASSIGN (change_gd-parameter) TO <fs_zrfclog>.
*        CHECK sy-subrc = 0 .
*        zilogkeystr-fdname = change_gd-parameter.
*        EXPORT <fs_zrfclog> TO DATABASE zfmdata(fl) ID zilogkeystr FROM wa_zilogdata.
*      ENDLOOP.
*
*      LOOP AT export_gd.
*        ASSIGN (export_gd-parameter) TO <fs_zrfclog>.
*        CHECK sy-subrc = 0 .
*        zilogkeystr-fdname = export_gd-parameter.
*        EXPORT <fs_zrfclog> TO DATABASE zfmdata(fl) ID zilogkeystr FROM wa_zilogdata.
*      ENDLOOP.
*
*      LOOP AT tables_gd.
*        CONCATENATE tables_gd-parameter '[]' INTO zilogfsstr.
*        ASSIGN (zilogfsstr) TO <fs_zrfclog>.
*        CHECK sy-subrc = 0 .
*        zilogkeystr-fdname = tables_gd-parameter.
*        EXPORT <fs_zrfclog> TO DATABASE zfmdata(fl) ID zilogkeystr FROM wa_zilogdata.
*      ENDLOOP.
*
*      IF import_gd[] IS INITIAL AND change_gd[] IS INITIAL AND
*         export_gd[] IS INITIAL AND tables_gd[] IS INITIAL.
*        EXPORT &1 TO DATABASE zfmdata(fl) ID zilogkeystr FROM wa_zilogdata.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*END-OF-DEFINITION.
  ENDMETHOD.
ENDCLASS.
