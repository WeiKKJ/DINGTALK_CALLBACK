*&---------------------------------------------------------------------*
*& Report ZABAP_LOG_SHOW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_log_show.
TABLES:zabap_log.
DATA: gt_fldct    TYPE lvc_t_fcat,
      gs_slayt    TYPE lvc_s_layo,
      gs_varnt    TYPE disvariant,
      gv_repid    TYPE sy-repid,
      gt_sort_lvc TYPE lvc_t_sort,
      gs_sort_lvc LIKE LINE OF gt_sort_lvc.

DATA: BEGIN OF gs_out,
        name   LIKE zabap_log-name,
        erdat  LIKE zabap_log-erdat,
        stamp  LIKE zabap_log-stamp,
        indx   LIKE zabap_log-indx,
        ernam  TYPE zabap_log-ernam,
        memo   TYPE zabap_log-memo,
        rtype  TYPE zabap_log-rtype,
        rtmsg  TYPE zabap_log-rtmsg,
        secds  TYPE zabap_log-secds,
        uterm  TYPE zabap_log-uterm,
        fdname TYPE zabap_log-fdname,
        sel,
      END OF gs_out.
DATA: gt_out LIKE TABLE OF gs_out.
DATA: BEGIN OF zilogkeystr,
        name   LIKE zabap_log-name,
        erdat  LIKE zabap_log-erdat,
        stamp  LIKE zabap_log-stamp,
        indx   LIKE zabap_log-indx,
        fdname LIKE zabap_log-fdname,
      END OF zilogkeystr.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE btxt1.
  PARAMETERS p_name TYPE zabap_log-name NO-DISPLAY.
  SELECT-OPTIONS s_erdat FOR gs_out-erdat DEFAULT sy-datum.
  SELECT-OPTIONS s_name FOR gs_out-name MEMORY ID zfmdataread_name.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  btxt1 = '数据筛选'(t01).

AT SELECTION-SCREEN. "PAI
  CASE sy-ucomm.
    WHEN 'ONLI'.
      PERFORM auth_check.
  ENDCASE.

INITIALIZATION.
  PERFORM catset TABLES gt_fldct USING:
'NAME  ' '' '' '记录的名称'     ,
'FDNAME' '' '' '事件类型'     ,
'ERDAT ' '' '' '日期'      ,
'STAMP ' '' '' '时间戳'     ,
'INDX  ' '' '' '计数器'     ,
'SECDS' '' '' '执行时间'    ,
'ERNAM' '' '' '用户名'      ,
'UTERM' '' '' '终端'      ,
'MEMO' '' '' '备注'      .
*'RTYPE' '' '' '消息类型'      ,
*'RTMSG' '' '' '消息文本'      .


START-OF-SELECTION.
  PERFORM getdata.
  PERFORM outdata.

*&---------------------------------------------------------------------*
*&      Form  auth_check
*&---------------------------------------------------------------------*
FORM auth_check.
*  AUTHORITY-CHECK OBJECT 'M_BEST_WRK'
*  ID 'ACTVT' DUMMY
*  ID 'WERKS' FIELD p_werks.
*  IF sy-subrc <> 0.
*    MESSAGE e000(oo) WITH '无工厂权限:'(m01) p_werks.
*  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& getdata
*&---------------------------------------------------------------------*
FORM getdata.
  CLEAR gt_out.
  SELECT
    name,
    erdat,
    stamp,
    indx,
    ernam,
    memo,
    rtype,
    rtmsg,
    secds,
    uterm,
    fdname
    FROM zabap_log
    WHERE name IN @s_name
    AND erdat IN @s_erdat
    GROUP BY name,erdat,stamp,indx,ernam,memo,rtype,rtmsg,secds,uterm,fdname
    ORDER BY name,erdat,stamp,indx,ernam,memo,rtype,rtmsg,secds,uterm,fdname
    INTO CORRESPONDING FIELDS OF TABLE @gt_out
    .

  IF gt_out IS INITIAL.
    MESSAGE s000(oo) WITH 'No Data'.
    EXIT.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* outdata
*---------------------------------------------------------------------*
FORM outdata.
  gv_repid        = sy-repid.
  gs_slayt-zebra  = 'X'.
  gs_slayt-box_fname  = 'SEL'.
  gs_varnt-report = sy-repid.
  gs_varnt-handle = 1.
  CLEAR:gt_sort_lvc,gs_sort_lvc.
  gs_sort_lvc-spos = 1.
  gs_sort_lvc-fieldname = 'NAME'.
  gs_sort_lvc-up = 'X'.
  APPEND gs_sort_lvc TO gt_sort_lvc.
  CLEAR:gs_sort_lvc.
  gs_sort_lvc-spos = 2.
  gs_sort_lvc-fieldname = 'ERDAT'.
  gs_sort_lvc-down = 'X'.
  APPEND gs_sort_lvc TO gt_sort_lvc.
  CLEAR:gs_sort_lvc.
  gs_sort_lvc-spos = 3.
  gs_sort_lvc-fieldname = 'STAMP'.
  gs_sort_lvc-down = 'X'.
  APPEND gs_sort_lvc TO gt_sort_lvc.
  CLEAR:gs_sort_lvc.
  gs_sort_lvc-spos = 4.
  gs_sort_lvc-fieldname = 'INDX'.
  gs_sort_lvc-up = 'X'.
  APPEND gs_sort_lvc TO gt_sort_lvc.
  CLEAR:gs_sort_lvc.
  gs_sort_lvc-spos = 5.
  gs_sort_lvc-fieldname = 'FDNAME'.
  gs_sort_lvc-up = 'X'.
  APPEND gs_sort_lvc TO gt_sort_lvc.
  CHECK gt_out IS NOT INITIAL.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      it_fieldcat_lvc          = gt_fldct
      i_save                   = 'A'
      is_variant               = gs_varnt
      is_layout_lvc            = gs_slayt
      i_callback_program       = gv_repid
      it_sort_lvc              = gt_sort_lvc
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SET_STATUS'
    TABLES
      t_outtab                 = gt_out.
ENDFORM.

*&---------------------------------------------------------------------*
*& set_status
*&---------------------------------------------------------------------*
FORM set_status USING pt_extab TYPE slis_t_extab ##CALLED.
  SET PF-STATUS 'STD_FULL' EXCLUDING pt_extab.
*  SET PF-STATUS 'STD_FULL'.
ENDFORM.

*&--------------------------------------------------------------------*
*& ALV user_command
*&--------------------------------------------------------------------*
FORM user_command USING pv_ucomm TYPE sy-ucomm ##CALLED
      pv_field TYPE slis_selfield.
  DATA:detail TYPE string.
  READ TABLE gt_out INTO gs_out INDEX pv_field-tabindex.
  CASE pv_ucomm.
    WHEN '&IC1'.
*      CASE pv_field-fieldname.
*        WHEN 'NAME'.
*          PERFORM display_se37 USING gs_out-name.
*        WHEN OTHERS.
*          CLEAR:zilogkeystr.
      MOVE-CORRESPONDING gs_out TO zilogkeystr.
*          CALL SCREEN 100.
*      ENDCASE.
      CLEAR:detail.
      TRY .
          IMPORT detail = detail FROM DATABASE zabap_log(fl) ID zilogkeystr.
          CHECK detail IS NOT INITIAL.
          CALL TRANSFORMATION sjson2html SOURCE XML detail
                                         RESULT XML DATA(html).

          cl_demo_output=>display_html(
            cl_abap_conv_codepage=>create_in( )->convert( html ) ).
        CATCH cx_sy_import_mismatch_error.
          RETURN.
      ENDTRY.
    WHEN 'TCLIP'.
      PERFORM alvtoclip IN PROGRAM zrpubform IF FOUND
      TABLES gt_out USING 'X'.
    WHEN 'RE_FRE'."刷新
      PERFORM getdata.
      pv_field-row_stable = 'X'.
      pv_field-col_stable = 'X'.
      pv_field-refresh    = 'X'.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
* set fieldcat
*---------------------------------------------------------------------*
FORM catset TABLES t_fldcat
USING pv_field pv_reftab pv_reffld pv_text.
  DATA: ls_fldcat TYPE lvc_s_fcat.

  ls_fldcat-fieldname =  pv_field.    "字段名
  ls_fldcat-scrtext_l =  pv_text.     "长描述
  ls_fldcat-coltext   =  pv_text.     "列描述
  ls_fldcat-ref_table =  pv_reftab.   "参考表名
  ls_fldcat-ref_field =  pv_reffld.   "参考字段名
  ls_fldcat-col_opt   = 'A'.          "自动优化列宽

  CASE ls_fldcat-fieldname.
    WHEN 'GSMNG'.
      ls_fldcat-qfieldname = 'MEINS'.
      ls_fldcat-no_zero    = 'X'.
    WHEN 'MENGE'.
      ls_fldcat-qfieldname = 'MEINS'.
      ls_fldcat-no_zero    = 'X'.
    WHEN 'WRBTR'.
      ls_fldcat-cfieldname = 'WAERS'.
    WHEN 'LIFNR' OR 'AUFNR' OR 'KUNNR'.
      ls_fldcat-edit_mask = '==ALPHA'.
    WHEN 'MATNR' OR 'IDNRK'.
      ls_fldcat-edit_mask = '==MATN1'.
    WHEN 'MEINS' .
      ls_fldcat-edit_mask = '==CUNIT'.
  ENDCASE.

  APPEND ls_fldcat TO t_fldcat.
  CLEAR ls_fldcat.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form display_se37
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_OUT_NAME
*&---------------------------------------------------------------------*
FORM display_se37  USING    p_name.
  DATA: ls_infoline1a TYPE vrsinfolna,
        ls_infoline1b TYPE vrsinfolnb,
        lv_report     TYPE  sy-repid.
  DATA:is_vrs_disp    TYPE        vrs_disp,
       iv_rfcdest     TYPE        rfcdest,
       iv_disp_report TYPE        sy-repid.
  DATA mv_objname TYPE vrsd-objname .
  DATA mv_objtype TYPE vrsd-objtype .
  iv_disp_report = 'RSVRSFU1'.
  IF iv_disp_report IS NOT INITIAL.
    lv_report = iv_disp_report.
  ELSE.
    MESSAGE s630(sb).
    RETURN.
*      mr_vers_db_access->get_display_report(
*      IMPORTING
*        ev_report = lv_report
*      ).
  ENDIF.

  mv_objname = p_name.
  mv_objtype = 'FUNC'.

  ls_infoline1a-objname = mv_objname.
  ls_infoline1b-korrnum = is_vrs_disp-korrno.
  WRITE is_vrs_disp-date TO ls_infoline1b-datum.
  ls_infoline1b-author = is_vrs_disp-usr.

  SUBMIT (lv_report) AND RETURN
        WITH objtype = mv_objtype
        WITH objname = mv_objname
        WITH versno  = is_vrs_disp-versnum
        WITH infolna = ls_infoline1a
        WITH infolnb = ls_infoline1b
        WITH rfcdest = is_vrs_disp-rfcdest.

  CLEAR: ls_infoline1a, ls_infoline1b.
ENDFORM.
