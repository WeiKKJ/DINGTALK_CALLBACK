class ZCL_DINGTALK_CALLBACK definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.

  data MY_PARAMS type TIHTTPNVP .
  data APPID type ZE_APPID value `2dce6c4b-8695-4a79-8c38-eb5be6633cfe` ##NO_TEXT.

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
            encrypt        TYPE string,
            encrypt_decode TYPE string,
          END OF t_JSON1.
    DATA:wa_encrypt TYPE t_JSON1.
    DATA:dingCryptode   TYPE REF TO zcl_dingtalk_callback_crypto.
    DATA:msg_signature TYPE string,
         timestamp     TYPE string,
         nonce         TYPE string,
         content       TYPE string,
         text          TYPE string.
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
*从配置表获取加密 aes_key、签名 token以及AppKey
    SELECT SINGLE * FROM ztddconfig WHERE appid = @appid INTO @DATA(wa_ztddconf).
    READ TABLE lt_header INTO DATA(wa_header) WITH KEY name = '~request_method' .
    CASE wa_header-value.
      WHEN 'GET'.
        DATA(msg) = `{"rtype": "S","rtmsg": "钉钉回调服务已启动","appid":"` && appid
        && `","appname":"` && wa_ztddconf-name && `","author":"kkw","mail":"weikj@foxmail.com"}`.
        http_msg msg.
      WHEN 'POST'.
*获取query参数
        READ TABLE lt_header INTO DATA(wa_params) WITH KEY name = '~query_string' .
        my_params = me->get_params( EXPORTING params = wa_params-value ).
        json = server->request->if_http_entity~get_cdata( ).
        /ui2/cl_json=>deserialize( EXPORTING json = json  pretty_name = /ui2/cl_json=>pretty_mode-low_case CHANGING data = wa_encrypt ).

        CLEAR:msg_signature,timestamp,nonce.
        READ TABLE my_params ASSIGNING FIELD-SYMBOL(<my_params>) WITH KEY name = 'signature'.
        IF sy-subrc EQ 0.
          msg_signature = <my_params>-value.
        ENDIF.
        READ TABLE my_params ASSIGNING <my_params> WITH KEY name = 'timestamp'.
        IF sy-subrc EQ 0.
          timestamp = <my_params>-value.
        ENDIF.
        READ TABLE my_params ASSIGNING <my_params> WITH KEY name = 'nonce'.
        IF sy-subrc EQ 0.
          nonce = <my_params>-value.
        ENDIF.
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
            msg_signature         = msg_signature
            timestamp             = timestamp
            nonce                 = nonce
            content               = wa_encrypt-encrypt
          RECEIVING
            text                  = text
          EXCEPTIONS
            signature_check_error = 1
            contentx_error        = 2
            padding_error         = 3
            appkey_error          = 4
            OTHERS                = 5.
        IF sy-subrc <> 0.
*   Implement suitable error handling here
        ENDIF.
        /ui2/cl_json=>deserialize( EXPORTING json = text  pretty_name = /ui2/cl_json=>pretty_mode-low_case CHANGING data = wa_event ).
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
ENDCLASS.
