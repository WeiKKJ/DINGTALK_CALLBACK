class ZCL_DINGTALK_CARD_CALLBACK definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  class-methods EZOUTTRACKID
    importing
      value(OUTTRACKID) type ZE_OUTTRACKID
      value(UNLOCK) type CHAR1
    returning
      value(RTMSG) type BAPI_MSG .
  class-methods RE_EXEC
    importing
      value(OUTTRACKID) type ZE_OUTTRACKID
    exporting
      value(RTYPE) type BAPI_MTYPE
      value(RTMSG) type BAPI_MSG .
protected section.
private section.

  data MY_PARAMS type TIHTTPNVP .

  methods GET_PARAMS
    importing
      !PARAMS type STRING
    returning
      value(MY_PARAMS) type TIHTTPNVP .
ENDCLASS.



CLASS ZCL_DINGTALK_CARD_CALLBACK IMPLEMENTATION.


  METHOD GET_PARAMS.
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
         res       TYPE string,
         proto     TYPE string,
         host      TYPE string,
         port      TYPE string.
    TYPES: BEGIN OF t_JSON1,
             extension            TYPE string,
             corp_id              TYPE string,
             out_track_id         TYPE string,
             value                TYPE string,
             open_conversation_id TYPE string,
             user_id              TYPE string,
             content              TYPE string,
           END OF t_JSON1.
    TYPES: t_ACTION_IDS2 TYPE string.
    TYPES: BEGIN OF t_PARAMS3,
             callback_key TYPE string,
             action       TYPE string,
           END OF t_PARAMS3.
    TYPES: tt_ACTION_IDS2 TYPE STANDARD TABLE OF t_ACTION_IDS2 WITH DEFAULT KEY.
    TYPES: BEGIN OF t_CARD_PRIVATE_DATA4,
             action_ids TYPE tt_ACTION_IDS2,
             params     TYPE t_PARAMS3,
           END OF t_CARD_PRIVATE_DATA4.
    TYPES: BEGIN OF t_JSON1_value,
             card_private_data TYPE t_CARD_PRIVATE_DATA4,
           END OF t_JSON1_value.
    DATA:wa_out       TYPE t_JSON1,
         wa_out_value TYPE t_JSON1_value.
    DATA: zilogt1 TYPE i,
          zilogt2 TYPE i,
          secds   TYPE zilogsecds.
*返回消息
    DEFINE http_msg.
      server->response->set_header_field( name = 'Content-Type' value = 'application/json;charset=utf-8' ).
      server->response->set_status( code = 200  reason = 'ok' ).
      server->response->set_cdata( EXPORTING data   = &1 ).
    END-OF-DEFINITION.

    CLEAR:lt_header,json.
    server->request->get_header_fields( CHANGING fields = lt_header ).
*从配置表获取加密 aes_key、签名 token以及AppKey
    SELECT SINGLE * FROM ztddconfig WHERE name LIKE '%SAP推送通知%' INTO @DATA(wa_ddconfig).
    CHECK NOT wa_ddconfig-cardtemplateid IS INITIAL OR NOT wa_ddconfig-openconversationid IS INITIAL OR NOT wa_ddconfig-callbackroutekey IS INITIAL.
    READ TABLE lt_header INTO DATA(wa_header) WITH KEY name = '~request_method' .
    CASE wa_header-value.
      WHEN 'GET'.
        DATA(msg) = `{"rtype": "S","rtmsg": "钉钉互动卡片回调服务已启动"}`.
        http_msg msg.
      WHEN 'POST'.
        GET RUN TIME FIELD zilogt1.
*获取query参数
        CLEAR:my_params,json.
        READ TABLE lt_header INTO DATA(wa_params) WITH KEY name = '~query_string' .
        my_params = me->get_params( EXPORTING params = wa_params-value ).
        json = server->request->if_http_entity~get_cdata( ).
        /ui2/cl_json=>deserialize( EXPORTING json = json  pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = wa_out ).
        /ui2/cl_json=>deserialize( EXPORTING json = wa_out-value pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = wa_out_value ).
        IF wa_out-out_track_id IS NOT INITIAL.
          CALL METHOD zcl_dingtalk_card_callback=>ezouttrackid
            EXPORTING
              outtrackid = CONV ze_outtrackid( wa_out-out_track_id )
              unlock     = ''
            RECEIVING
              rtmsg      = DATA(rtmsg).

          IF rtmsg IS NOT INITIAL.
            res = `{"cardData":{"cardParamMap":{"result":"` && rtmsg && `","rtype":"E"}},"userIdType":1,"cardOptions":{"updateCardDataByKey":true,"updatePrivateDataByKey":false}}`.
          ELSE.
            CASE wa_out_value-card_private_data-params-action.
              WHEN 'reexec'.
                IF sy-mandt = '800'.
                  res = `{"cardData":{"cardParamMap":{"butStatus":"0","but02Text":"暂不启用该功能","result":"暂不启用该功能` && rtmsg && `","rtype":"S"}},"userIdType":1,"cardOptions":{"updateCardDataByKey":true,"updatePrivateDataByKey":false}}`.
                ELSE.
                  CALL METHOD zcl_dingtalk_card_callback=>re_exec
                    EXPORTING
                      outtrackid = CONV ze_outtrackid( wa_out-out_track_id )
                    IMPORTING
*                     rtype      =
                      rtmsg      = rtmsg.
                  IF rtmsg CS '成功'.
                    res = `{"cardData":{"cardParamMap":{"butStatus":"0","but02Text":"已执行完毕","result":"回调成功，` && rtmsg && `","rtype":"S"}},"userIdType":1,"cardOptions":{"updateCardDataByKey":true,"updatePrivateDataByKey":false}}`.
                  ELSE.
                    res = `{"cardData":{"cardParamMap":{"result":"回调成功，` && rtmsg && `","rtype":"S"}},"userIdType":1,"cardOptions":{"updateCardDataByKey":true,"updatePrivateDataByKey":false}}`.
                  ENDIF.
                ENDIF.
              WHEN 'cancel'.
                res = `{"cardData":{"cardParamMap":{"butStatus":"0","but02Text":"已取消执行","result":"取消成功","rtype":"S"}},"userIdType":1,"cardOptions":{"updateCardDataByKey":true,"updatePrivateDataByKey":false}}`.
              WHEN OTHERS.
                res = `{"cardData":{"cardParamMap":{"result":"请求不被支持","rtype":"E"}},"userIdType":1,"cardOptions":{"updateCardDataByKey":true,"updatePrivateDataByKey":false}}`.
            ENDCASE.
          ENDIF.
        ELSE.
          res = `{"cardData":{"cardParamMap":{"result":"请求不被支持","rtype":"E"}},"userIdType":1,"cardOptions":{"updateCardDataByKey":true,"updatePrivateDataByKey":false}}`.
        ENDIF.
        http_msg res.

        CALL METHOD zcl_dingtalk_callback=>add_log
          EXPORTING
            name       = CONV rs38l_fnam( 'ZDT_CARD_CB' )
            eventtype  = CONV rs38l_par_( 'KKW' )
            detail_ori = json
            detail     = res
            secds      = secds.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD ezouttrackid.
    IF unlock NE 'X'."加锁.
      CALL FUNCTION 'ENQUEUE_EZOUTTRACKID'
        EXPORTING
*         MODE_ZSOUTTRACKID       = 'E'
*         MANDT          = SY-MANDT
          outtrackid     = outtrackid
*         X_OUTTRACKID   = ' '
          _scope         = '1'
*         _WAIT          = ' '
*         _COLLECT       = ' '
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        SELECT SINGLE *
          FROM usrefus
          WHERE bname = @sy-msgv1
          INTO @DATA(l_usrefus)
          .
        rtmsg = |用户{ sy-msgv1 }({ l_usrefus-useralias })正在处理卡片{ outtrackid }|.
      ENDIF.
    ELSE.
      CALL FUNCTION 'DEQUEUE_EZOUTTRACKID'
        EXPORTING
*         MODE_ZSOUTTRACKID       = 'E'
*         MANDT      = SY-MANDT
          outtrackid = outtrackid
*         X_OUTTRACKID            = ' '
          _scope     = '1'
*         _SYNCHRON  = ' '
*         _COLLECT   = ' '
        .
    ENDIF.
  ENDMETHOD.


  METHOD re_exec.
    DATA:seltab TYPE TABLE OF rsparams.
    SELECT
      *
      FROM zabap_log
      WHERE outtrackid = @outtrackid
      AND relid = 'DT'
      AND memo = 'DT'
      INTO TABLE @DATA(lt_log)
      .
    DESCRIBE TABLE lt_log LINES DATA(line).
    IF line EQ 0.
      rtype = 'E'.
      rtmsg = '无可执行的的记录'.
      RETURN.
    ELSEIF line NE 1.
      rtype = 'E'.
      rtmsg = |{ outtrackid }不唯一|.
      RETURN.
    ENDIF.
    READ TABLE lt_log ASSIGNING FIELD-SYMBOL(<lt_log>) INDEX 1.
    INSERT INITIAL LINE INTO TABLE seltab ASSIGNING FIELD-SYMBOL(<seltab>).
    <seltab>-selname = 'S_ERDAT'.
    <seltab>-kind    = 'S'.
    <seltab>-sign    = 'I'.
    <seltab>-option  = 'EQ'.
    <seltab>-low     = <lt_log>-erdat.
    <seltab>-high    = ''.

    INSERT INITIAL LINE INTO TABLE seltab ASSIGNING <seltab>.
    <seltab>-selname = 'S_STAMP'.
    <seltab>-kind    = 'S'.
    <seltab>-sign    = 'I'.
    <seltab>-option  = 'EQ'.
    <seltab>-low     = <lt_log>-stamp.
    <seltab>-high    = ''.

    INSERT INITIAL LINE INTO TABLE seltab ASSIGNING <seltab>.
    <seltab>-selname = 'S_NAME'.
    <seltab>-kind    = 'S'.
    <seltab>-sign    = 'I'.
    <seltab>-option  = 'EQ'.
    <seltab>-low     = <lt_log>-name.
    <seltab>-high    = ''.

    INSERT INITIAL LINE INTO TABLE seltab ASSIGNING <seltab>.
    <seltab>-selname = 'P_OUTID'.
    <seltab>-kind    = 'P'.
    <seltab>-sign    = 'I'.
    <seltab>-option  = 'EQ'.
    <seltab>-low     = outtrackid.
    <seltab>-high    = ''.
    DATA oo TYPE ze_outtrackid.
    IMPORT outtrackid = oo FROM MEMORY ID 'ZRFC_DATA_READ-OUTTRACKID'.
    IF sy-subrc EQ 0.
      FREE MEMORY ID 'ZRFC_DATA_READ-OUTTRACKID'.
    ENDIF.
    EXPORT outtrackid = outtrackid TO MEMORY ID 'ZRFC_DATA_READ-OUTTRACKID'.
*    SUBMIT zrfc_data_read
*      WITH SELECTION-TABLE seltab
*      AND RETURN
*        .
    IMPORT rtmsg = rtmsg FROM MEMORY ID 'ZRFC_DATA_READ-RTMSG'.
  ENDMETHOD.
ENDCLASS.
