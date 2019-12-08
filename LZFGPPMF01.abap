*----------------------------------------------------------------------*
***INCLUDE LZFGPPMF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F01_FILL_SELECT_FIELDS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SELECT_FIELDS
*&---------------------------------------------------------------------*
FORM f01_fill_select_fields

TABLES it_select_fields TYPE rsfs_fields.
*----------------------------------------------------------------------*

  DATA: wa_select_fields TYPE rsfs_tab_fields.
  DATA: wa_fields_line   LIKE rsfs_struc.
* refresh table
  CLEAR   it_select_fields.
  REFRESH it_select_fields.
* project definition
  wa_select_fields-tablename   = 'PROJ'.
  wa_fields_line-line          = 'PSPNR'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'PSPID'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'POST1'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'OBJNR'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'VBUKR'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  APPEND wa_select_fields      TO it_select_fields.
  REFRESH wa_select_fields-fields.

* sales document
  wa_select_fields-tablename   = 'VBAP'.
  wa_fields_line-line          = 'VBELN'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'POSNR'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'MATNR'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'OBJNR'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'ARKTX'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  APPEND wa_select_fields      TO it_select_fields.
  REFRESH wa_select_fields-fields.

* wbs element
  wa_select_fields-tablename   = 'PRPS_R'.
  wa_fields_line-line          = 'PSPNR'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'POSID'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'PSPHI'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'POST1'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'OBJNR'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'PBUKR'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'IMPRF'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  APPEND wa_select_fields      TO it_select_fields.
  REFRESH wa_select_fields-fields.

* order / network
  wa_select_fields-tablename   = 'AUFK'.
  wa_fields_line-line          = 'AUFNR'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'AUART'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'AUTYP'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'KTEXT'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'BUKRS'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'PSPEL'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  APPEND wa_select_fields      TO it_select_fields.
  REFRESH wa_select_fields-fields.

* network activity
  wa_select_fields-tablename   = 'ACT01'.
  wa_fields_line-line          = 'AUFPL'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'APLZL'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'OBJNR'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'VORNR'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'LTXA1'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'BUKRS'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  wa_fields_line-line          = 'PROJN'.
  APPEND wa_fields_line        TO wa_select_fields-fields.
  APPEND wa_select_fields      TO it_select_fields.
  REFRESH wa_select_fields-fields.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F01_SET_RSPARAMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_RSPARAMS
*&      --> P_PROJECT
*&      --> LV_DATE_TO
*&      --> LV_LAST_DATE
*&---------------------------------------------------------------------*
FORM f01_set_rsparams  USING    p_date_from
                                p_date_to
                      CHANGING p_lt_rsparams TYPE rsparams_tt
                               p_lt_wrttp    TYPE ps05_range_t_wrttp.

  DATA: lw_rsparams TYPE rsparams,
        lv_profile  TYPE char12 VALUE '000000000001',
        lv_comp_prj TYPE char17,
        lw_wrttp    LIKE LINE OF p_lt_wrttp.




  LOOP AT p_lt_rsparams ASSIGNING FIELD-SYMBOL(<fs_rsparams>).
    CASE <fs_rsparams>-selname.
      WHEN 'R_BUDAT' .
        <fs_rsparams>-kind = 'S'.
        <fs_rsparams>-sign = 'I'.
        <fs_rsparams>-option = 'BT'.
        <fs_rsparams>-low = p_date_from.
        <fs_rsparams>-high = p_date_to.
      WHEN 'CN_PROJN' .
        <fs_rsparams>-kind = 'S'.
        <fs_rsparams>-sign = 'I'.
        <fs_rsparams>-option = 'EQ'.
        <fs_rsparams>-low = gv_project.
      WHEN 'CN_PROFD' .
        <fs_rsparams>-kind = 'P'.
        <fs_rsparams>-low  = lv_profile.
      WHEN 'CN_MAXL' .
        <fs_rsparams>-kind = 'P'.
        <fs_rsparams>-low  = '99'.
      WHEN 'CN_KOKRS' .
        <fs_rsparams>-kind = 'P'.
        <fs_rsparams>-low  = '1058'.
      WHEN 'CN_STUFE' .
        <fs_rsparams>-kind = 'S'.
        <fs_rsparams>-sign = 'I'.
        <fs_rsparams>-option = 'BT'.
        <fs_rsparams>-low = '1'.
        <fs_rsparams>-high = '99'.
      WHEN 'G_WRTTP' .
        <fs_rsparams>-kind = 'S'.
        <fs_rsparams>-sign = 'I'.
        <fs_rsparams>-option = 'EQ'.
        <fs_rsparams>-low = '04'.
      WHEN 'P_ACTVRS' .
        <fs_rsparams>-kind = 'P'.
        <fs_rsparams>-low  = '000'.
      WHEN 'P_USEDB' .
        <fs_rsparams>-kind = 'P'.
        <fs_rsparams>-low  = 'X'.
      WHEN 'CN_DBDAT' .
        <fs_rsparams>-kind = 'P'.
        <fs_rsparams>-low  = 'X'.
      WHEN 'CN_AKTDT' .
        <fs_rsparams>-kind = 'P'.
        <fs_rsparams>-low  = 'X'.
      WHEN 'CN_AMODI' .
        <fs_rsparams>-kind = 'P'.
        <fs_rsparams>-low  = 'X'.
      WHEN 'CN_VSEL1' .
        <fs_rsparams>-kind = 'P'.
        <fs_rsparams>-low  = 'X'.
      WHEN 'P_MAXSEL' .
        <fs_rsparams>-kind = 'P'.
        <fs_rsparams>-low  = '5000'.
      WHEN 'P_DIVAR' .
        <fs_rsparams>-kind = 'P'.
        <fs_rsparams>-low  = '1SAP'.
      WHEN 'P_NOZERO' .
        <fs_rsparams>-kind = 'P'.
        <fs_rsparams>-low  = 'X'.

    ENDCASE.

  ENDLOOP.

  lw_rsparams-selname = 'G_WRTTP'.
  lw_rsparams-kind = 'S'.
  lw_rsparams-sign = 'I'.
  lw_rsparams-option = 'EQ'.
  lw_rsparams-low = '11'.


  APPEND lw_rsparams TO p_lt_rsparams.

  lw_wrttp-sign = 'I'.
  lw_wrttp-option = 'EQ'.
  lw_wrttp-low = '11'.

  APPEND lw_wrttp TO p_lt_wrttp.


  lw_wrttp-sign = 'I'.
  lw_wrttp-option = 'EQ'.
  lw_wrttp-low = '04'.

  APPEND lw_wrttp TO p_lt_wrttp.

  SORT p_lt_rsparams BY selname ASCENDING low.


ENDFORM.
**&---------------------------------------------------------------------*
**& Form CREATE_SORTED_ELM_PS
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> GT_ELM_PS[]
**&---------------------------------------------------------------------*
*FORM create_sorted_elm_ps    TABLES it_elm_ps STRUCTURE elm_ps.
*
*
** start of note 2248252
*  DATA: ls_elm_ps       LIKE LINE OF it_elm_ps,
*        ls_elm_ps_objnr LIKE LINE OF gt_elm_ps_objnr.
*
*  LOOP AT it_elm_ps INTO ls_elm_ps.
*    ls_elm_ps_objnr-objnr = ls_elm_ps-objnr.
*    ls_elm_ps_objnr-line  = sy-tabix.
*    APPEND ls_elm_ps_objnr TO gt_elm_ps_objnr.
*  ENDLOOP.
*  SORT gt_elm_ps_objnr BY objnr.
** end   of note 2248252
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  F01_RSTHIE_M_CORR
**&---------------------------------------------------------------------*
*FORM f01_rsthie_m_corr TABLES t_rsthie_m STRUCTURE rsthie_m.
*  DATA: ld_objnr LIKE onr00-objnr.                          "P99K050989
** korrigiere Objektnummer für Vertriebsbelegkopf und -position
** LOOP AT T_RSTHIE_M WHERE TYPE = 'VK  ' OR TYPE = 'VB  '.  "P99K050989
*  LOOP AT t_rsthie_m.
*    IF t_rsthie_m-type = 'VK  '.
*      t_rsthie_m-name+0(2) = 'VK'.
*      MODIFY t_rsthie_m.                                    "P99K050989
*    ELSEIF t_rsthie_m-type = 'VB  '.
*      t_rsthie_m-name+0(2) = 'VB'.
*      MODIFY t_rsthie_m.                                    "P99K050989
*    ELSEIF t_rsthie_m-type <> 'PD  ' AND                    "P99K050989
*           t_rsthie_m-type <> 'V1  ' AND                    "P99K050989
*           t_rsthie_m-type <> 'ID  ' AND                    "note 2408496: investment program
*           t_rsthie_m-type <> 'IP  '.                       "note 2408496: invest- prgm. position ID
*      ld_objnr = t_rsthie_m-name.                           "P99K050989
*      CALL FUNCTION 'HFPM_GET_TYPE_OF_OBJECT'               "P99K050989
*           EXPORTING                                        "P99K050989
*                objnr                   = ld_objnr          "P99K050989
*           EXCEPTIONS                                       "P99K050989
*                wrong_obart             = 1                 "P99K050989
*                OTHERS                  = 2.                "P99K050989
*      IF sy-subrc <> 0.                                     "P99K050989
*        DELETE t_rsthie_m.                                  "P99K050989
*      ENDIF.                                                "P99K050989
*    ENDIF.
**   MODIFY T_RSTHIE_M.                                      "P99K050989
*  ENDLOOP.
*
*ENDFORM.                               " F01_RSTHIE_M_CORR
*---------------------------------------------------------------------*
FORM f01_fill_fields_for_selection
*----------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
TABLES   it_rsparams           STRUCTURE rsparams
         it_wrttp              TYPE      ps05_range_t_wrttp
         it_rsthie_m           STRUCTURE rsthie_m
         et_cosel              STRUCTURE cosel
         et_pd_selection       STRUCTURE cosel
*----------------------------------------------------------------------*
USING    VALUE(v_called)       TYPE      c
         VALUE(v_actvrs)       LIKE      covp-versn
*----------------------------------------------------------------------*
CHANGING c_disvariant          TYPE      kaep_disvariant_key
         c_kokrs               LIKE      coep-kokrs
         ct_groups             TYPE      kaep_t_groups      "P99K042671
         c_fixvar              TYPE      c.
*----------------------------------------------------------------------*

  RANGES: lr_kstar     FOR  cskb-kstar.
  DATA: ld_sign_apc LIKE rkact_cl-sign_ap,
        ld_koagr    LIKE rkmah-hname,
        ld_obart    LIKE rsthie_m-type,
        lt_cosel    LIKE cosel    OCCURS 0 WITH HEADER LINE.
  DATA:   ls_memory_id TYPE kaep_memory_id.

* fill cosel from rsparams
  LOOP AT it_rsparams.
    CHECK NOT it_rsparams-low  IS INITIAL OR
          NOT it_rsparams-high IS INITIAL.
    CLEAR: et_cosel.
    CASE it_rsparams-selname.
      WHEN 'CN_KOKRS'.
        c_kokrs = it_rsparams-low.
      WHEN 'R_KSTAR'.
        MOVE-CORRESPONDING it_rsparams TO lr_kstar.
        APPEND lr_kstar.
      WHEN 'KOAGR'.
        ld_koagr = it_rsparams-low.
      WHEN 'R_ZHLDT'.
        MOVE-CORRESPONDING it_rsparams TO et_cosel.
        et_cosel-field = 'ZHLDT'.
        APPEND et_cosel.
        SET PARAMETER ID 'KT1' FIELD it_rsparams-low.
        SET PARAMETER ID 'KT2' FIELD it_rsparams-high.
      WHEN 'R_BUDAT' OR 'R_BUDAT1' OR 'R_BUDAT2'.
        MOVE-CORRESPONDING it_rsparams TO et_cosel.
        et_cosel-field = 'BUDAT'.
        APPEND et_cosel.
        SET PARAMETER ID 'KS7' FIELD it_rsparams-low.
        SET PARAMETER ID 'KS8' FIELD it_rsparams-high.
      WHEN 'R_VERSN'.
        MOVE-CORRESPONDING it_rsparams TO et_cosel.
        et_cosel-field = 'VERSN'.
        APPEND et_cosel.
        SET PARAMETER ID 'KVT' FIELD it_rsparams-low.
      WHEN 'R_VERSA'.
        MOVE-CORRESPONDING it_rsparams TO et_cosel.
        et_cosel-field = 'VERSN'.
        APPEND et_cosel.
        SET PARAMETER ID 'ACV' FIELD it_rsparams-low.
      WHEN 'R_PERIO'.
        MOVE-CORRESPONDING it_rsparams TO et_cosel.
        et_cosel-field = 'PERIO'.
        APPEND et_cosel.
        SET PARAMETER ID 'VPE' FIELD it_rsparams-low.
        SET PARAMETER ID 'BPE' FIELD it_rsparams-high.
      WHEN 'R_GJAHR' OR 'R_GJAHR1'.
        MOVE-CORRESPONDING it_rsparams TO et_cosel.
        et_cosel-field = 'GJAHR'.
        APPEND et_cosel.
        SET PARAMETER ID 'GJR' FIELD it_rsparams-low.
      WHEN 'P_GESW'.
        MOVE-CORRESPONDING it_rsparams TO et_cosel.
        et_cosel-field = 'FLGJG'.
        APPEND et_cosel.
      WHEN 'R_OBDAT'.
        MOVE-CORRESPONDING it_rsparams TO et_cosel.
        et_cosel-field = 'BUDAT'.
        APPEND et_cosel.
        SET PARAMETER ID 'KSY' FIELD it_rsparams-low.
        SET PARAMETER ID 'KSZ' FIELD it_rsparams-high.
      WHEN 'P_OPEN'.
        et_cosel-field  = 'WKGBTR'.
        et_cosel-sign   = 'I'.
        et_cosel-option = 'NE'.
        et_cosel-low    = '0'.
        APPEND et_cosel.
      WHEN 'P_FIXVAR'.
        c_fixvar = it_rsparams-low.
      WHEN 'P_AFABE'.
        MOVE-CORRESPONDING it_rsparams TO et_cosel.
        et_cosel-field = 'AFABE'.
        APPEND et_cosel.
*      when 'P_ACTVRS'.
      WHEN OTHERS.
*       nothing to do
    ENDCASE.
  ENDLOOP.

  READ TABLE it_wrttp WITH KEY low = '21'.
  IF sy-subrc <> 0.
*   except for commitment report:
*   get year and period from posting date (-> performance!)
    PERFORM f01_get_year_period TABLES et_cosel
                                USING  c_kokrs.
  ENDIF.
* only for actual and commitment: get valuation type and its versions
  CALL FUNCTION 'K_LINE_ITEM_APC_SIGN_GET'
    EXPORTING
      i_item_group = c_disvariant-handle
    IMPORTING
      e_sign_apc   = ld_sign_apc.
  IF ld_sign_apc = con_act OR
     ld_sign_apc = con_cmt.
    PERFORM f01_get_actual_versions TABLES et_cosel
                                    USING  v_actvrs
                                           c_kokrs.
  ENDIF.
* check if range for cost elements or cost element group is not initial
  IF NOT lr_kstar[] IS INITIAL OR
     NOT ld_koagr   IS INITIAL.
    PERFORM f01_get_cost_elements TABLES   lr_kstar
                                           lt_cosel
                                  USING    c_kokrs
*                                          LD_KOAGR.        "P99K042671
                                           ld_koagr         "P99K042671
                                  CHANGING ct_groups[].     "P99K042671
    APPEND LINES OF lt_cosel TO et_cosel.
    REFRESH lt_cosel.
  ENDIF.
* fill PS ledger for budget line items into cosel
  IF c_disvariant-handle = con_item_group-budget    OR
     c_disvariant-handle = con_item_group-strc_plan.
    PERFORM f01_get_ps_ledger USING    c_kokrs
                              CHANGING lt_cosel.
    APPEND lt_cosel TO et_cosel.
  ENDIF.
* add value types to selection table
  CLEAR et_cosel.
  et_cosel-field  = 'WRTTP'.
  LOOP AT it_wrttp.
    MOVE-CORRESPONDING it_wrttp TO et_cosel.
    APPEND et_cosel.
  ENDLOOP.
* only one object type for header infotmations (rsthie must be filled)
  READ TABLE it_rsthie_m INDEX 1.
  ld_obart = it_rsthie_m-type.
* fill cosel with objects
  CLEAR et_cosel.
  LOOP AT it_rsthie_m.
* start of note 2408496: Exclude investment program from selection criteria
    IF it_rsthie_m-name(2) = 'ID' OR
       it_rsthie_m-name(2) = 'IP'.
      CONTINUE.
    ENDIF.
* end of note 2408496
    et_cosel-field  = 'OBJNR'.
    et_cosel-sign   = 'I'.
    et_cosel-option = 'EQ'.
    et_cosel-low    = it_rsthie_m-name.
    APPEND et_cosel.
* fill cosel for header (highest level only)
    IF   ld_obart = it_rsthie_m-type   AND         "same object type
       ( it_rsthie_m-tlevel IS INITIAL OR          "not called by rri
       ( it_rsthie_m-tlevel = '01'     AND         "    called by rri
         v_called = con_on ) ).
      APPEND et_cosel TO et_pd_selection.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
FORM f01_get_actual_versions
*&---------------------------------------------------------------------*
TABLES   ct_cosel                 STRUCTURE cosel
*----------------------------------------------------------------------*
USING    VALUE(v_actual_version)  LIKE      covp-versn
         VALUE(v_kokrs)           LIKE      tka01-kokrs.
*----------------------------------------------------------------------*
  RANGES: lr_versn   FOR covp-versn,
          lr_valutyp FOR tka09-valutyp.
  DATA: lt_versn   LIKE coep-versn OCCURS 0 WITH HEADER LINE,
        ls_tka09   LIKE tka09,
        ld_valutyp LIKE tcvprofd-valutyp.

* get valuation
  CALL FUNCTION 'K_VERSN_READ'
    EXPORTING
      i_kokrs = v_kokrs
      i_versn = v_actual_version
    IMPORTING
      e_tka09 = ls_tka09.
  ld_valutyp = ls_tka09-valutyp.
* get actual versions for transfer price valuation
  CALL FUNCTION 'K_VERSN_FOR_VALUATION'
    EXPORTING
      i_kokrs                = v_kokrs
    TABLES
      t_versn                = lt_versn
    CHANGING
      c_valutyp              = ld_valutyp
    EXCEPTIONS
      no_versn_for_valuation = 1
      OTHERS                 = 2.
  IF sy-subrc = 0.
    READ TABLE lt_versn WITH KEY v_actual_version.
    IF sy-subrc = 0.
*     actual version is a transfer price valuation version -> get others
      LOOP AT lt_versn.
        lr_versn-sign = 'I'.
        lr_versn-option = 'EQ'.
        lr_versn-low = lt_versn.
        MOVE-CORRESPONDING lr_versn TO ct_cosel.
        ct_cosel-field = 'VERSN'.
        COLLECT ct_cosel.
      ENDLOOP.
      lr_valutyp-sign = 'I'.
      lr_valutyp-option = 'EQ'.
      lr_valutyp-low = ld_valutyp.
      MOVE-CORRESPONDING lr_valutyp TO ct_cosel.
      ct_cosel-field = 'VALUTYP'.
      APPEND ct_cosel.
    ELSE.
      lr_versn-sign = 'I'.
      lr_versn-option = 'EQ'.
      lr_versn-low = v_actual_version.
      MOVE-CORRESPONDING lr_versn TO ct_cosel.
      ct_cosel-field = 'VERSN'.
      APPEND ct_cosel.
    ENDIF.
  ELSE.
    lr_versn-sign = 'I'.
    lr_versn-option = 'EQ'.
    lr_versn-low = v_actual_version.
    MOVE-CORRESPONDING lr_versn TO ct_cosel.
    ct_cosel-field = 'VERSN'.
    APPEND ct_cosel.
  ENDIF.

ENDFORM.                               " GET_ACTUAL_VERSION
*----------------------------------------------------------------------*
FORM f01_get_cost_elements
*----------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
TABLES   ir_kstar       TYPE      ps05_range_t_kstar
         et_cosel       STRUCTURE cosel
*----------------------------------------------------------------------*
USING    VALUE(i_kokrs) LIKE      coep-kokrs
*        VALUE(I_KOAGR) TYPE      PS05_KOAGR.               "P99K042671
         VALUE(i_koagr) TYPE      ps05_koagr                "P99K042671
CHANGING ct_groups      TYPE      kaep_t_groups.            "P99K042671
*----------------------------------------------------------------------*
  DATA: ls_tka01    LIKE tka01,
*       LT_GROUPS   TYPE KAEP_T_GROUPS,                     "P99K042671
        lt_roselect LIKE cosel    OCCURS 0 WITH HEADER LINE,
        lt_cosel    LIKE cosel    OCCURS 0 WITH HEADER LINE.

* Ranges noch verdichten (disjunkte Werte) !!!!!!!!!!!!
  IF NOT i_koagr IS INITIAL.

    SELECT * FROM tka01 INTO ls_tka01
      WHERE kokrs = i_kokrs.
    ENDSELECT.

    PERFORM dissolve_set(rkaep000)
                         TABLES   lt_cosel[]
                         USING    i_koagr
                                  gsetc_costelement_setclass
                                  con_field-kstar
                                  i_kokrs
                                  ls_tka01-ktopl
*                        CHANGING LT_GROUPS[].              "P99K042671
                         CHANGING ct_groups[].              "P99K042671
  ENDIF.
  LOOP AT ir_kstar.
    CLEAR lt_roselect.
    lt_roselect-field = 'KSTAR'.
    MOVE-CORRESPONDING ir_kstar TO lt_roselect.
    APPEND lt_roselect.
  ENDLOOP.

  LOOP AT lt_cosel.
    CLEAR lt_roselect.
    lt_roselect-field = 'KSTAR'.
    MOVE-CORRESPONDING lt_cosel TO lt_roselect.
    APPEND lt_roselect.
  ENDLOOP.

  CALL FUNCTION 'ROMU_RS_CONVERT_ICSEL_TO_COSEL'
    TABLES
      i_selection_table = lt_roselect
      i_cosel           = et_cosel
    EXCEPTIONS
      OTHERS            = 1.

  IF sy-subrc NE 0.
    REFRESH et_cosel.
  ENDIF.

ENDFORM.                               " f01_GET_COST_ELEMENTS
*----------------------------------------------------------------------*
FORM f01_get_ps_ledger
*&---------------------------------------------------------------------*
*       delivers lednr for kokrs currency
*----------------------------------------------------------------------*
USING    VALUE(i_kokrs) LIKE coep-kokrs
*----------------------------------------------------------------------*
CHANGING cs_cosel       LIKE cosel.
*----------------------------------------------------------------------*

  DATA: ld_lednr LIKE rpsco-lednr.

  CLEAR cs_cosel.
  CALL FUNCTION 'HFPB_FIND_LEDGER'
    EXPORTING
      kokrs     = i_kokrs
      owaer     = ' '
    IMPORTING
      lednr_k   = ld_lednr
    EXCEPTIONS
      no_ledger = 1.
  IF sy-subrc IS INITIAL.
    cs_cosel-field  = 'LEDNR'.
    cs_cosel-sign   = 'I'.
    cs_cosel-option = 'EQ'.
    cs_cosel-low    = ld_lednr.
  ENDIF.

ENDFORM.                               " F01_GET_PS_LEDGER
*&---------------------------------------------------------------------*
*&      Form  F01_GET_YEAR_PERIOD
*&---------------------------------------------------------------------*
FORM f01_get_year_period TABLES   ct_cosel STRUCTURE cosel
                         USING    VALUE(p_kokrs) LIKE tka01-kokrs.

  DATA: ld_gjahr_low  LIKE cobk-gjahr,
        ld_gjahr_high LIKE cobk-gjahr,
        ld_perio_low  LIKE covp-perio,
        ld_perio_high LIKE covp-perio,
        ld_budat      LIKE cobk-budat,
        ld_anzbp      LIKE t009-anzbp,
        ld_anzsp      LIKE t009-anzsp.
  DATA: ld_kokrs LIKE tka01-kokrs,
        ls_tka00 LIKE tka00,
        ls_tka01 LIKE tka01.

* check whether mapping can be done
  CHECK: NOT p_kokrs IS INITIAL.
  READ TABLE ct_cosel WITH KEY field = 'BUDAT'.
  CHECK: sy-subrc IS INITIAL.

  ld_kokrs = p_kokrs.
  PERFORM f01_get_controlling_area CHANGING ld_kokrs
                                            ls_tka00
                                            ls_tka01.
  ld_budat = ct_cosel-low.
  CALL FUNCTION 'G_PERIOD_GET'
    EXPORTING
      date    = ld_budat
      variant = ls_tka01-lmona
    IMPORTING
      anzbp   = ld_anzbp
      anzsp   = ld_anzsp
      period  = ld_perio_low
      year    = ld_gjahr_low
    EXCEPTIONS
      OTHERS  = 1.

  IF sy-subrc = 0.

    IF ct_cosel-high IS INITIAL OR
       ct_cosel-high = '00000000'.              "note 1164418
      ld_budat = ct_cosel-low.
    ELSE.
      ld_budat = ct_cosel-high.
    ENDIF.

* determine highest normal period
    CALL FUNCTION 'G_PERIODS_OF_YEAR_GET'
      EXPORTING
        variant             = ls_tka01-lmona
        year                = ld_gjahr_low
      IMPORTING
        last_normal_period  = ld_anzbp
      EXCEPTIONS
        variant_not_defined = 1
        year_not_defined    = 2
        OTHERS              = 3.


    CALL FUNCTION 'G_PERIOD_GET'
      EXPORTING
        date    = ld_budat
        variant = ls_tka01-lmona
      IMPORTING
        period  = ld_perio_high
        year    = ld_gjahr_high
      EXCEPTIONS
        OTHERS  = 1.

    IF sy-subrc = 0.
      CLEAR ct_cosel.
      ct_cosel-field  = 'GJAHR'.
      ct_cosel-sign   = 'I'.
      IF ld_gjahr_low <> ld_gjahr_high AND NOT ld_gjahr_high IS INITIAL.
*       fiscal year range: no selection with period
        ct_cosel-option = 'BT'.
        ct_cosel-low    = ld_gjahr_low.
        ct_cosel-high   = ld_gjahr_high.
        APPEND ct_cosel.
      ELSE.
*       single fiscal year: selection with period too
        ct_cosel-option = 'EQ'.
        ct_cosel-low    = ld_gjahr_low.
        APPEND ct_cosel.
        CLEAR ct_cosel.
        ct_cosel-field  = 'PERIO'.
        ct_cosel-sign   = 'I'.
        IF ld_perio_high = ld_anzbp AND
           ld_anzsp > 0.
*         last period: add special periods
          ld_perio_high = '992'.
          ct_cosel-option = 'BT'.
          ct_cosel-low    = ld_perio_low.
          ct_cosel-high   = ld_perio_high.
        ELSEIF ld_perio_low = ld_perio_high OR ld_perio_high IS INITIAL.
*         single period
          ct_cosel-option = 'EQ'.
          ct_cosel-low    = ld_perio_low.
        ELSE.
*         period range without special periods
          ct_cosel-option = 'BT'.
          ct_cosel-low    = ld_perio_low.
          ct_cosel-high   = ld_perio_high.
        ENDIF.
        APPEND ct_cosel.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " F01_GET_YEAR_PERIOD
*----------------------------------------------------------------------*
FORM f01_get_controlling_area
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
CHANGING c_kokrs LIKE coep-kokrs
         c_tka00 LIKE tka00
         c_tka01 LIKE tka01.
*----------------------------------------------------------------------*
  DATA: lb_gui   TYPE answer,
        ld_popup TYPE c.

  IF NOT c_kokrs IS INITIAL.
    SET PARAMETER ID 'CAC' FIELD c_kokrs.
  ELSE.
    GET PARAMETER ID 'CAC' FIELD c_kokrs.
    CALL FUNCTION 'RFC_CF_IS_GUI_ON'
      IMPORTING
        on = lb_gui.
    IF c_kokrs IS INITIAL.
      IF NOT ( sy-batch IS INITIAL AND lb_gui = 'Y' ).
        ld_popup = '0'.
      ENDIF.
      CALL FUNCTION 'K_KOKRS_SET'
        EXPORTING
          display   = ' '
          i_kokrs   = ' '
          popup     = ld_popup
        IMPORTING
          e_kokrs   = c_kokrs
        EXCEPTIONS
          not_found = 01.

      IF NOT sy-subrc IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'K_KOKRS_READ'
    EXPORTING
      kokrs   = c_kokrs
    IMPORTING
      e_tka00 = c_tka00
      e_tka01 = c_tka01.


ENDFORM.                               " F01_GET_CONTROLLING_AREA
*&---------------------------------------------------------------------*
*& Form F_CHECK_PROJECT_DEFINITION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_check_project_definition CHANGING p_return TYPE bapiret2_t.

  SELECT pspid FROM proj INTO gv_project UP TO 1 ROWS
    WHERE pspid = gv_project.
  ENDSELECT.

  IF sy-subrc IS NOT INITIAL.
    CALL FUNCTION 'HRIQ_APPEND_MESSAGE_TABLE'
      EXPORTING
        iv_msgtype   = 'E'
        iv_msgclass  = 'ZCOPPMCOST'
        iv_msgnumber = 000
      CHANGING
        ct_return    = p_return.

    RETURN.
  ENDIF.


*Duration in seconds for 500 executions: 0,0103690

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_EMPTY_DATE_TO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PERIOD_TO
*&      --> YEAR_TO
*&      <-- LV_DATE_TO
*&---------------------------------------------------------------------*
FORM f_set_empty_date_to CHANGING p_date_to.

  CALL FUNCTION 'HR_HK_ADD_MONTH_TO_DATE'
    EXPORTING
      dmm_datin = sy-datum
      dmm_count = '1'
      dmm_oper  = '-'
      dmm_pos   = abap_true
    IMPORTING
      dmm_daout = p_date_to
    EXCEPTIONS
      unknown   = 1
      OTHERS    = 2.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_EMPTY_DATE_FROM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_DATE_FROM
*&---------------------------------------------------------------------*
FORM f_set_empty_date_from  CHANGING p_date_from.

  CALL FUNCTION 'HR_HK_ADD_MONTH_TO_DATE'
    EXPORTING
      dmm_datin = sy-datum
      dmm_count = '2'
      dmm_oper  = '-'
      dmm_pos   = abap_true
    IMPORTING
      dmm_daout = p_date_from
    EXCEPTIONS
      unknown   = 1
      OTHERS    = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_COST_ELEMENT_DESCRIPTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_cost_element_description CHANGING p_cost_element_description .

  SELECT saknr , txt50 INTO TABLE @DATA(lt_cost_element_d)
    FROM skat FOR ALL ENTRIES IN @gt_ppm_cost  WHERE spras =  @c_language AND
                                                     ktopl =  @c_chart    AND
                                                     saknr =  @gt_ppm_cost-kstar.

  IF sy-subrc IS INITIAL.
    p_cost_element_description = lt_cost_element_d.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TCNDB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_TCNB
*&---------------------------------------------------------------------*
FORM set_tcndb  CHANGING p_ls_tcnb TYPE tcndb.

  p_ls_tcnb-mandt        = sy-mandt.
  p_ls_tcnb-profid       = '1'.
  p_ls_tcnb-aenam        = 'SAP'.
  p_ls_tcnb-protect      = abap_true.
  p_ls_tcnb-custom       = abap_true.
  p_ls_tcnb-proj         = abap_true.
  p_ls_tcnb-prps         = abap_true.
  p_ls_tcnb-hiekz        = abap_true.
  p_ls_tcnb-plaf         = abap_true.
  p_ls_tcnb-netz_psp     = abap_true.
  p_ls_tcnb-netz         = abap_true.
  p_ls_tcnb-act_psp      = abap_true.
  p_ls_tcnb-act          = abap_true.
  p_ls_tcnb-komp         = abap_true.
  p_ls_tcnb-maxlevel     = '99'.
  p_ls_tcnb-db_view      = '1'.
  p_ls_tcnb-pfad_oben    = abap_true.
  p_ls_tcnb-corep        = abap_true.
  p_ls_tcnb-incnt        = abap_true.
  p_ls_tcnb-incvg        = abap_true.
  p_ls_tcnb-incih        = abap_true.
  p_ls_tcnb-incihvg      = abap_true.
  p_ls_tcnb-incor        = abap_true.
  p_ls_tcnb-incorvg      = abap_true.
  p_ls_tcnb-incihsub     = abap_true.
  p_ls_tcnb-incco        = abap_true.
  p_ls_tcnb-incpp        = abap_true.
  p_ls_tcnb-incppvg      = abap_true.
  p_ls_tcnb-inctn        = abap_true.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_DATE_INTERVAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_DATE_FROM
*&      --> LV_DATE_TO
*&---------------------------------------------------------------------*
FORM f_check_date_interval  USING    p_lv_date_from
                                     p_lv_date_to
  CHANGING p_return TYPE bapiret2_t.


  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = p_lv_date_from
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    CALL FUNCTION 'HRIQ_APPEND_MESSAGE_TABLE'
      EXPORTING
        iv_msgtype   = 'E'
        iv_msgclass  = 'ZCOPPMCOST'
        iv_msgnumber = 003
      CHANGING
        ct_return    = p_return.

    RETURN.

  ENDIF.

  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = p_lv_date_to
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    CALL FUNCTION 'HRIQ_APPEND_MESSAGE_TABLE'
      EXPORTING
        iv_msgtype   = 'E'
        iv_msgclass  = 'ZCOPPMCOST'
        iv_msgnumber = 003
      CHANGING
        ct_return    = p_return.

    RETURN.

  ENDIF.

  IF p_lv_date_from > p_lv_date_to.

    CALL FUNCTION 'HRIQ_APPEND_MESSAGE_TABLE'
      EXPORTING
        iv_msgtype   = 'E'
        iv_msgclass  = 'ZCOPPMCOST'
        iv_msgnumber = 002
      CHANGING
        ct_return    = p_return.

    RETURN.


  ENDIF.


ENDFORM.