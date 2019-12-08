*-------------------------------------------------------------------
***INCLUDE LKAEPS01 .
*-------------------------------------------------------------------

*&---------------------------------------------------------------------*
FORM select
*&---------------------------------------------------------------------*
USING VALUE(vt_selection)       TYPE kaep_t_cosel
      VALUE(vt_free_selection)  TYPE rsds_twhere
      VALUE(v_fix_variant)      LIKE kaep_sett-fixvar
      VALUE(v_maxsel)           LIKE kaep_sett-maxsel.

*&---------------------------------------------------------------------*
  CALL FUNCTION 'ZK_LINE_ITEMS_SELECT'
    EXPORTING
      i_pos_table           = gd-pos_db_table
      it_nametab_pos        = gt_nametab_pos
      i_head_table          = gd-head_db_table
      it_nametab_head       = gt_nametab_head
      is_keyinfo            = gs_keyinfo
      i_select_form         = gd-select_form
      it_obligatory_fields  = gt_obligatory_fields
      it_sel_depend         = gt_sel_depend
      i_co_area             = gd-kokrs
      it_selection          = vt_selection
      it_free_selection     = vt_free_selection
      i_fieldcat_select     = v_fix_variant
      i_online_maxsel       = v_maxsel
      it_sort               = gt_sortinfo[]
    IMPORTING
      et_objnr              = gt_objnr[]
      et_kstar              = gt_kstar[]
      et_matnr              = gt_matnr[]
      et_gkont              = gt_gkont[]
      et_ebeln              = gt_ebeln[]
    CHANGING
      ct_fieldcat           = gt_variant_fieldcat[]
      it_kaepx_data_carrier = gt_kaepx_data_carrier
    EXCEPTIONS
      OTHERS                = 1 .

ENDFORM.                               "SELECT
* new table
* You have to enter a form called 'Select_xxx' (xxx = table name)
*&---------------------------------------------------------------------*
FORM select_covp
*&---------------------------------------------------------------------*
*     text                                                           *
*----------------------------------------------------------------------*
USING VALUE(v_callback_programm) LIKE sy-repid
      VALUE(vt_var_cond)         TYPE kaep_t_var_cond
      VALUE(vt_fields)           TYPE kaep_t_fieldname
      VALUE(v_db_table)          TYPE kaep_tabname.
*----------------------------------------------------------------------*
  RANGES lr_kstar_opt FOR covp-kstar.

  DATA: ls_covp              LIKE covp,
        lr_objnr             LIKE gr_objnr OCCURS 0,
        ld_sel_lines         LIKE sy-tfill VALUE 0,
        ld_obj_lines         LIKE sy-tfill,
        ld_bloc_tab          TYPE kaep_fieldname,
        ld_bloc_lines        LIKE sy-tfill VALUE 0,
        ld_blocs             LIKE sy-tfill VALUE 0,
        ld_exit              TYPE kaep_flag,
        ld_exit_sel          TYPE kaep_flag,
        max_obj_bloc         TYPE i,
        min_number_of_blocs  TYPE sy-tfill VALUE 0,
        ls_t811flags         LIKE t811flags,
        ld_max_sel_lines     LIKE sy-tfill VALUE 45,
        max_number_kstar_opt TYPE i VALUE 0,
        ls_kstar_opt         LIKE lr_kstar_opt,
        lr_kstar             LIKE gr_kstar OCCURS 0,
        lv_auditor           TYPE c.                   "note 1607963-1/3
  DATA: ld_fields             TYPE string.

* definitions for delta versions
  DATA: lt_cossa LIKE cossa OCCURS 0 WITH HEADER LINE.

*.new KAEPX functionality
  IF NOT gt_kaepx_data_carrier[] IS INITIAL.
    READ TABLE gt_kaepx_data_carrier INTO
               gs_kaepx_data_carrier INDEX 1.
*....check if KAEPX-relevant data is handed over
    MOVE-CORRESPONDING gs_kaepx_data_carrier TO
         gs_kaepx_relevant.
    IF NOT gs_kaepx_relevant IS INITIAL.
      PERFORM select_covp_kaepx USING vt_var_cond
                                      vt_fields
                                      v_db_table.
      EXIT.
    ENDIF.
  ENDIF.

  PERFORM add_lines_count TABLES   gr_gjahr
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_versn
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_perio
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_budat
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_parob
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_wrttp
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   vt_var_cond
                          CHANGING ld_sel_lines.

*.check if number of select options is changed
  SELECT SINGLE * FROM t811flags INTO ls_t811flags
               WHERE tab   = 'KAEP'
                 AND field = 'CON_MAX_SEL_LINES'.
  IF sy-subrc = 0 AND
     ls_t811flags-valmin <> space.
    ld_max_sel_lines = ls_t811flags-valmin.
  ENDIF.

  IF ld_sel_lines >= ld_max_sel_lines.
    ld_bloc_lines = 1.
  ELSE.
    ld_bloc_lines = ld_max_sel_lines - ld_sel_lines.
  ENDIF.

*.new logic to select partner object
  IF NOT gr_objnr[] IS INITIAL.
    ld_fields = |OBJNR IN LR_OBJNR|.
  ENDIF.
  IF NOT gr_parob1[] IS INITIAL AND
     gr_objnr[] IS INITIAL.
    ld_fields = |PAROB1 IN LR_OBJNR|.
    gr_objnr[] = gr_parob1[].
  ENDIF.
  IF NOT gr_uspob[] IS INITIAL AND
     gr_objnr[] IS INITIAL.
    ld_fields = |USPOB IN LR_OBJNR|.
    gr_objnr[] = gr_uspob[].
  ELSEIF NOT gr_uspob[] IS INITIAL AND                      "2719419
         NOT gr_objnr[] IS INITIAL.                         "2719419
    ld_fields = |OBJNR IN LR_OBJNR AND USPOB IN GR_USPOB|.  "2719419
  ENDIF.

*.check if number of object block is changed
  SELECT SINGLE * FROM t811flags INTO ls_t811flags
               WHERE tab   = 'KAEP'
                 AND field = 'MAX_OBJ_BLOC'.
  IF sy-subrc = 0 AND
     ls_t811flags-valmin <> space.
    max_obj_bloc = ls_t811flags-valmin.
  ELSE.
    max_obj_bloc = 1.
  ENDIF.
* maximum size of object block for single select
* MAX_OBJ_BLOC = 1.
  DESCRIBE TABLE gr_objnr LINES ld_obj_lines.
  DESCRIBE TABLE gr_kstar LINES sy-tfill.

*.check if cost element opt is activated
  SELECT SINGLE * FROM t811flags INTO ls_t811flags
               WHERE tab   = 'KAEP'
                 AND field = 'MAX_NUMBER_KSTAR_OPT'.
  IF sy-subrc = 0 AND
     ls_t811flags-valmin <> space.
    max_number_kstar_opt = ls_t811flags-valmin.
  ENDIF.

* optimization of cost element ranges
  IF max_number_kstar_opt < sy-tfill AND
     max_number_kstar_opt NE 0.
    CLEAR lr_kstar_opt.
    lr_kstar_opt[] = gr_kstar[].
    LOOP AT lr_kstar_opt INTO ls_kstar_opt.
      IF ls_kstar_opt-option = 'EQ'.
        ls_kstar_opt-option = 'BT'.
        ls_kstar_opt-high   = ls_kstar_opt-low.
        MODIFY lr_kstar_opt FROM ls_kstar_opt.
      ENDIF.
    ENDLOOP.
    CLEAR: gr_kstar[], ls_kstar_opt.
    ls_kstar_opt-sign = 'I'.
    ls_kstar_opt-option = 'BT'.
    SORT lr_kstar_opt BY high DESCENDING.
    READ TABLE lr_kstar_opt INDEX 1 INTO ls_kstar_opt TRANSPORTING high.
    SORT lr_kstar_opt BY low.
    READ TABLE lr_kstar_opt INDEX 1 INTO ls_kstar_opt TRANSPORTING low.
    APPEND ls_kstar_opt TO gr_kstar.
    DESCRIBE TABLE gr_kstar LINES sy-tfill.
  ENDIF.

* bloc larger table
  IF ld_obj_lines > sy-tfill AND max_obj_bloc > sy-tfill.
    PERFORM clc_blocs USING ld_obj_lines max_obj_bloc
                   CHANGING min_number_of_blocs.
    ld_bloc_tab = con_field-objnr.
    sy-tfill = ld_obj_lines.
  ELSE.
    ld_bloc_tab = con_field-kstar.
  ENDIF.

  IF sy-tfill > ld_bloc_lines.
    PERFORM clc_blocs USING    sy-tfill
                               ld_bloc_lines
                      CHANGING ld_blocs.
  ELSE.
    ld_blocs = 1.
  ENDIF.

  IF min_number_of_blocs > ld_blocs.
    ld_blocs = min_number_of_blocs.
    ld_bloc_lines = max_obj_bloc.
  ENDIF.

* Start of note 1607963-2/3: Zeitraumprüfung
* check if user is an auditor
  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      funcname           = 'CA_USER_EXISTS'
    EXCEPTIONS
      function_not_exist = 1
      OTHERS             = 2.
  IF sy-subrc = 0.
    CALL FUNCTION 'CA_USER_EXISTS'
      EXPORTING
        i_user       = sy-uname
      EXCEPTIONS
        user_missing = 1.
    IF sy-subrc = 0.
      lv_auditor = 'X'.
    ENDIF.
  ENDIF.
* End of note 1607963-2/3: Zeitraumprüfung

  DO.

    CASE ld_bloc_tab.
      WHEN con_field-kstar.
        REFRESH lr_objnr.
        READ TABLE gr_objnr INDEX sy-index.
        IF sy-subrc <> 0.
*         no select without object: exit immediately
          EXIT.
        ELSE.
          APPEND gr_objnr TO lr_objnr.
        ENDIF.
      WHEN con_field-objnr.
        REFRESH lr_kstar.
        IF gr_kstar[] IS INITIAL.
*         one select without cost element possible: exit after select
          ld_exit_sel = 'X'.
        ELSE.
          READ TABLE gr_kstar INDEX sy-index.
          IF sy-subrc <> 0.
            EXIT.
          ELSE.
            APPEND gr_kstar TO lr_kstar.
          ENDIF.
        ENDIF.
    ENDCASE.

*note 2178343: replacing view COVP with views with less functionality
    DATA: lv_db_table     TYPE tabname,
          lv_db_table_act TYPE tabname.

    IF v_db_table = 'COVP'.

      IF '04' IN gr_wrttp.                                  "2672882
*     check if view FOR ACTUALS shall be changed
        CLEAR ls_t811flags.
        SELECT SINGLE * FROM t811flags INTO ls_t811flags
                     WHERE tab   = 'KAEP'
                       AND field = 'VIEW_FOR_COVP_ACT'.
        IF sy-subrc = 0 AND
           ls_t811flags-valmin <> space.
*     special view for actuals from COVP shall be used
*     this view will be called in a separate SELECT
          lv_db_table_act = ls_t811flags-valmin.
          CALL FUNCTION 'DDIF_NAMETAB_GET'
            EXPORTING
              tabname   = lv_db_table_act
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          IF sy-subrc <> 0.
            CLEAR lv_db_table_act.
          ENDIF.
        ENDIF.
      ENDIF.                                                "2672882

*     check if view FOR ALL shall be changed
      CLEAR ls_t811flags.
      SELECT SINGLE * FROM t811flags INTO ls_t811flags
                   WHERE tab   = 'KAEP'
                     AND field = 'VIEW_FOR_COVP'.
      IF sy-subrc = 0 AND
         ls_t811flags-valmin <> space.
*     special view for non-actuals from COVP shall be used in addition
        lv_db_table = ls_t811flags-valmin.
        CALL FUNCTION 'DDIF_NAMETAB_GET'
          EXPORTING
            tabname   = lv_db_table
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc <> 0.
          CLEAR lv_db_table.
        ENDIF.
      ELSE.
*     if only 'VIEW_FOR_COVP_ACT' is prepared use that view for all selections.
        lv_db_table = lv_db_table_act.
        CLEAR lv_db_table_act.
      ENDIF.

      IF lv_db_table IS NOT INITIAL.
        v_db_table = lv_db_table.
      ENDIF.


      RANGES: lr_wrttp_act FOR covp-wrttp.

      IF lv_db_table_act IS NOT INITIAL.
*      exclude WRTTP = 04 from general valuetype for the second select below.
        lr_wrttp_act-sign = 'E'. "exclude!
        lr_wrttp_act-option = 'EQ'.
        lr_wrttp_act-low = '04'.
        APPEND lr_wrttp_act TO gr_wrttp.
*       include value type 04 to the first select
        FREE lr_wrttp_act.
        lr_wrttp_act-sign = 'I'. "include
        lr_wrttp_act-option = 'EQ'.
        lr_wrttp_act-low = '04'.
        APPEND lr_wrttp_act TO lr_wrttp_act.
      ENDIF.

    ENDIF. "Replace COVP

    DO ld_blocs TIMES.
      CASE ld_bloc_tab.
        WHEN con_field-kstar.
          PERFORM fill_bloc TABLES lr_kstar
                                   gr_kstar
                            USING  sy-index
                                   ld_bloc_lines.
        WHEN con_field-objnr.
          PERFORM fill_bloc TABLES lr_objnr
                                   gr_objnr
                            USING  sy-index
                                   ld_bloc_lines.
      ENDCASE.

*     note 2178343: performance improvements
*     Selection on extra view for actuals (WRTTP = 04)
      IF lv_db_table_act IS NOT INITIAL.

        SELECT (vt_fields) INTO CORRESPONDING FIELDS OF gs_covp_ext
                           FROM (lv_db_table_act)
                           WHERE lednr = '00'
                           AND   (ld_fields)
                           AND   gjahr IN gr_gjahr
                           AND   versn IN gr_versn
                           AND   wrttp IN lr_wrttp_act
                           AND   kstar IN lr_kstar
                           AND   perio IN gr_perio
                           AND   budat IN gr_budat
                           AND   parob IN gr_parob
                           AND   (vt_var_cond).
          CHECK gs_covp_ext-kokrs = gd-kokrs.
*         optimization of cost element ranges
          CHECK gs_covp_ext-kstar IN lr_kstar_opt.
*   Satrt of note 1607963-3/3: Zeitraumprüfung
          IF lv_auditor = 'X' AND sy-cprog = 'RKPEP003'.
            CALL FUNCTION 'CA_CHECK_DATE'
              EXPORTING
                i_applk           = 'PS-REP'
                i_orgunit         = gs_covp_ext-bukrs
                i_user            = sy-uname
                i_program         = sy-cprog
                i_from_date       = gs_covp_ext-budat
              EXCEPTIONS
                no_authority_date = 1
                no_authority_prog = 2
                wrong_parameter   = 3
                OTHERS            = 4.
            CHECK sy-subrc = 0.
          ENDIF.
*   End of note 1607963-3/3: Zeitraumprüfung
*         delta version
          REFRESH: lt_cossa.
          MOVE-CORRESPONDING gs_covp_ext TO lt_cossa.
          CALL FUNCTION 'K_DELTA_VERSNS_CHECK_ENTRY'
            EXPORTING
              db_struct = lt_cossa
              tabname   = 'COSSA'
            TABLES
              db_table  = lt_cossa
            EXCEPTIONS
              OTHERS    = 0.
          LOOP AT lt_cossa.
            MOVE-CORRESPONDING lt_cossa TO gs_covp_ext.
            APPEND gs_covp_ext TO gt_covp_ext.
          ENDLOOP.
*         collect master keys into global tables
          MOVE-CORRESPONDING gs_covp_ext TO ls_covp.
          PERFORM collect_master_keys_covp USING ls_covp.
          PERFORM gui_counter TABLES gt_covp_ext
                              CHANGING ld_exit.
          IF NOT ld_exit IS INITIAL.
            EXIT.
          ENDIF.
        ENDSELECT.
      ENDIF. "note 2178343

      SELECT (vt_fields) INTO CORRESPONDING FIELDS OF gs_covp_ext
                         FROM (v_db_table)
                         WHERE lednr = '00'
                         AND   (ld_fields)
                         AND   gjahr IN gr_gjahr
                         AND   versn IN gr_versn
                         AND   wrttp IN gr_wrttp
                         AND   kstar IN lr_kstar
                         AND   perio IN gr_perio
                         AND   budat IN gr_budat
                         AND   parob IN gr_parob
                         AND   (vt_var_cond).
        CHECK gs_covp_ext-kokrs = gd-kokrs.
*       optimization of cost element ranges
        CHECK gs_covp_ext-kstar IN lr_kstar_opt.
* Satrt of note 1607963-3/3: Zeitraumprüfung
        IF lv_auditor = 'X' AND sy-cprog = 'RKPEP003'.
          CALL FUNCTION 'CA_CHECK_DATE'
            EXPORTING
              i_applk           = 'PS-REP'
              i_orgunit         = gs_covp_ext-bukrs
              i_user            = sy-uname
              i_program         = sy-cprog
              i_from_date       = gs_covp_ext-budat
            EXCEPTIONS
              no_authority_date = 1
              no_authority_prog = 2
              wrong_parameter   = 3
              OTHERS            = 4.
          CHECK sy-subrc = 0.
        ENDIF.
* End of note 1607963-3/3: Zeitraumprüfung
*       delta version
        REFRESH: lt_cossa.
        MOVE-CORRESPONDING gs_covp_ext TO lt_cossa.
        CALL FUNCTION 'K_DELTA_VERSNS_CHECK_ENTRY'
          EXPORTING
            db_struct = lt_cossa
            tabname   = 'COSSA'
          TABLES
            db_table  = lt_cossa
          EXCEPTIONS
            OTHERS    = 0.
        LOOP AT lt_cossa.
          MOVE-CORRESPONDING lt_cossa TO gs_covp_ext.
          APPEND gs_covp_ext TO gt_covp_ext.
        ENDLOOP.
*       collect master keys into global tables
        MOVE-CORRESPONDING gs_covp_ext TO ls_covp.
        PERFORM collect_master_keys_covp USING ls_covp.
        PERFORM gui_counter TABLES gt_covp_ext
                            CHANGING ld_exit.
        IF NOT ld_exit IS INITIAL.
          EXIT.
        ENDIF.
      ENDSELECT.
      IF NOT ld_exit IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
    IF NOT ld_exit IS INITIAL OR NOT ld_exit_sel IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                               " SELECT_COVP

*&---------------------------------------------------------------------*
FORM select_covpb
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
USING VALUE(v_callback_programm) LIKE sy-repid
      VALUE(vt_var_cond)         TYPE kaep_t_var_cond
      VALUE(vt_fields)           TYPE kaep_t_fieldname
      VALUE(v_db_table)          TYPE kaep_tabname.
*----------------------------------------------------------------------*

  DATA: ls_covpb      LIKE covpb,
        lr_objnr      LIKE gr_objnr OCCURS 0,
        ld_sel_lines  LIKE sy-tfill VALUE 0,
        ld_obj_lines  LIKE sy-tfill VALUE 0,
        ld_bloc_lines LIKE sy-tfill VALUE 0,
        ld_bloc_tab   TYPE kaep_fieldname,
        ld_blocs      LIKE sy-tfill VALUE 0,
        ld_exit       TYPE kaep_flag,
        ld_exit_sel   TYPE kaep_flag,
        lr_kstar      LIKE gr_kstar OCCURS 0.

  PERFORM add_lines_count TABLES   gr_gjahr
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_versn
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_perio
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_budat
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_parob
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_wrttp
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   vt_var_cond
                          CHANGING ld_sel_lines.

  IF ld_sel_lines >= con_max_sel_lines.
    ld_bloc_lines = 1.
  ELSE.
    ld_bloc_lines = con_max_sel_lines - ld_sel_lines.
  ENDIF.

  DESCRIBE TABLE gr_objnr LINES ld_obj_lines.
  DESCRIBE TABLE gr_kstar LINES sy-tfill.

* bloc larger table
  IF ld_obj_lines > sy-tfill.
    ld_bloc_tab = con_field-objnr.
    sy-tfill = ld_obj_lines.
  ELSE.
    ld_bloc_tab = con_field-kstar.
  ENDIF.

  IF sy-tfill > ld_bloc_lines.
    PERFORM clc_blocs USING    sy-tfill
                               ld_bloc_lines
                      CHANGING ld_blocs.
  ELSE.
    ld_blocs = 1.
  ENDIF.

  DO.

    CASE ld_bloc_tab.
      WHEN con_field-kstar.
        REFRESH lr_objnr.
        READ TABLE gr_objnr INDEX sy-index.
        IF sy-subrc <> 0.
*         no select without object: exit immediately
          EXIT.
        ELSE.
          APPEND gr_objnr TO lr_objnr.
        ENDIF.
      WHEN con_field-objnr.
        REFRESH lr_kstar.
        IF gr_kstar[] IS INITIAL.
*         one select without cost element possible: exit after select
          ld_exit_sel = 'X'.
        ELSE.
          READ TABLE gr_kstar INDEX sy-index.
          IF sy-subrc <> 0.
            EXIT.
          ELSE.
            APPEND gr_kstar TO lr_kstar.
          ENDIF.
        ENDIF.
    ENDCASE.

    DO ld_blocs TIMES.
      CASE ld_bloc_tab.
        WHEN con_field-kstar.
          PERFORM fill_bloc TABLES lr_kstar
                                   gr_kstar
                            USING  sy-index
                                   ld_bloc_lines.
        WHEN con_field-objnr.
          PERFORM fill_bloc TABLES lr_objnr
                                   gr_objnr
                            USING  sy-index
                                   ld_bloc_lines.
      ENDCASE.

      SELECT (vt_fields) INTO CORRESPONDING FIELDS OF gs_covpb_ext
                         FROM (v_db_table)
                         WHERE lednr = '00'
                         AND   objnr IN lr_objnr
                         AND   gjahr IN gr_gjahr
                         AND   versn IN gr_versn
                         AND   wrttp IN gr_wrttp
                         AND   kstar IN lr_kstar
                         AND   perio IN gr_perio
                         AND   budat IN gr_budat
                         AND   parob IN gr_parob
                         AND   (vt_var_cond).
        CHECK gs_covpb_ext-kokrs = gd-kokrs.
        APPEND gs_covpb_ext TO gt_covpb_ext.
*.....collect master keys into global tables
        MOVE-CORRESPONDING gs_covpb_ext TO ls_covpb.
        PERFORM collect_master_keys_covpb USING ls_covpb.
        PERFORM gui_counter TABLES gt_covpb_ext
                            CHANGING ld_exit.
        IF NOT ld_exit IS INITIAL.
          EXIT.
        ENDIF.
      ENDSELECT.
      IF NOT ld_exit IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
    IF NOT ld_exit IS INITIAL OR NOT ld_exit_sel IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                               " SELECT_COVPB

*---------------------------------------------------------------------*
FORM select_covj
*---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
USING VALUE(v_callback_programm) LIKE sy-repid
      VALUE(vt_var_cond)         TYPE kaep_t_var_cond
      VALUE(vt_fields)           TYPE kaep_t_fieldname
      VALUE(v_db_table)          TYPE kaep_tabname.
*----------------------------------------------------------------------*

  DATA: ls_covj             LIKE covj,
        lr_objnr            LIKE gr_objnr OCCURS 0,
        ld_sel_lines        LIKE sy-tfill VALUE 0,
        ld_obj_lines        LIKE sy-tfill VALUE 0,
        ld_bloc_tab         TYPE kaep_fieldname,
        ld_bloc_lines       LIKE sy-tfill VALUE 0,
        ld_blocs            LIKE sy-tfill VALUE 0,
        ld_exit             TYPE kaep_flag,
        ld_exit_sel         TYPE kaep_flag,
        max_obj_bloc        TYPE i,
        min_number_of_blocs LIKE sy-tfill VALUE 0,
        lr_kstar            LIKE gr_kstar OCCURS 0.
  DATA: ld_fields             TYPE kaep_fieldname.
*.....definitions for delta versions
  DATA: lt_cossa LIKE cossa OCCURS 0 WITH HEADER LINE.

  IF generated_periods NE con_percl.
    MESSAGE x499(kb) WITH 'SELECT_COVJ(SAPLKAEP)'.
  ENDIF.

*.new KAEPX functionality
  IF NOT gt_kaepx_data_carrier[] IS INITIAL.
    READ TABLE gt_kaepx_data_carrier INTO
               gs_kaepx_data_carrier INDEX 1.
*....check if KAEPX-relevant data is handed over
    MOVE-CORRESPONDING gs_kaepx_data_carrier TO
         gs_kaepx_relevant.
    IF NOT gs_kaepx_relevant IS INITIAL.
      PERFORM select_covj_kaepx USING vt_var_cond
                                      vt_fields
                                      v_db_table.
      EXIT.
    ENDIF.
  ENDIF.

  PERFORM add_lines_count TABLES   gr_gjahr
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_versn
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_parob
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_wrttp
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   vt_var_cond
                          CHANGING ld_sel_lines.

  IF ld_sel_lines >= con_max_sel_lines.
    ld_bloc_lines = 1.
  ELSE.
    ld_bloc_lines = con_max_sel_lines - ld_sel_lines.
  ENDIF.

*.new logic to select partner object
  IF NOT gr_objnr[] IS INITIAL.
    ld_fields = |OBJNR IN LR_OBJNR|.
  ENDIF.
  IF NOT gr_parob1[] IS INITIAL AND
     gr_objnr[] IS INITIAL.
    ld_fields = |PAROB1 IN LR_OBJNR|.
    gr_objnr[] = gr_parob1[].
  ENDIF.
  IF NOT gr_uspob[] IS INITIAL AND
     gr_objnr[] IS INITIAL.
    ld_fields = |USPOB IN LR_OBJNR|.
    gr_objnr[] = gr_uspob[].
  ENDIF.

  max_obj_bloc = 1.

  DESCRIBE TABLE gr_objnr LINES ld_obj_lines.
  DESCRIBE TABLE gr_kstar LINES sy-tfill.

* bloc larger table
  IF ld_obj_lines > sy-tfill AND max_obj_bloc > sy-tfill.
    PERFORM clc_blocs USING ld_obj_lines max_obj_bloc
                   CHANGING min_number_of_blocs.
    ld_bloc_tab = con_field-objnr.
    sy-tfill = ld_obj_lines.
  ELSE.
    ld_bloc_tab = con_field-kstar.
  ENDIF.

  IF sy-tfill > ld_bloc_lines.
    PERFORM clc_blocs USING    sy-tfill
                               ld_bloc_lines
                      CHANGING ld_blocs.
  ELSE.
    ld_blocs = 1.
  ENDIF.

  IF min_number_of_blocs > ld_blocs.
    ld_blocs = min_number_of_blocs.
    ld_bloc_lines = max_obj_bloc.
  ENDIF.

  DO.

    CASE ld_bloc_tab.
      WHEN con_field-kstar.
        REFRESH lr_objnr.
        READ TABLE gr_objnr INDEX sy-index.
        IF sy-subrc <> 0.
*         no select without object: exit immediately
          EXIT.
        ELSE.
          APPEND gr_objnr TO lr_objnr.
        ENDIF.
      WHEN con_field-objnr.
        REFRESH lr_kstar.
        IF gr_kstar[] IS INITIAL.
*         one select without cost element possible: exit after select
          ld_exit_sel = 'X'.
        ELSE.
          READ TABLE gr_kstar INDEX sy-index.
          IF sy-subrc <> 0.
            EXIT.
          ELSE.
            APPEND gr_kstar TO lr_kstar.
          ENDIF.
        ENDIF.
    ENDCASE.

    DO ld_blocs TIMES.
      CASE ld_bloc_tab.
        WHEN con_field-kstar.
          PERFORM fill_bloc TABLES lr_kstar
                                   gr_kstar
                            USING  sy-index
                                   ld_bloc_lines.
        WHEN con_field-objnr.
          PERFORM fill_bloc TABLES lr_objnr
                                   gr_objnr
                            USING  sy-index
                                   ld_bloc_lines.
      ENDCASE.

      SELECT (vt_fields) INTO CORRESPONDING FIELDS OF gs_covja_ext
                         FROM (v_db_table)
                         WHERE lednr = '00'
                         AND   (ld_fields)
                         AND   gjahr IN gr_gjahr
                         AND   wrttp IN gr_wrttp
                         AND   versn IN gr_versn
                         AND   kstar IN lr_kstar
                         AND   perbl = con_percl
                         AND   parob IN gr_parob
                         AND   (vt_var_cond).
        CHECK gs_covja_ext-kokrs = gd-kokrs.
*.....delta version
        REFRESH: lt_cossa.
        MOVE-CORRESPONDING gs_covja_ext TO lt_cossa.
        CALL FUNCTION 'K_DELTA_VERSNS_CHECK_ENTRY'
          EXPORTING
            db_struct = lt_cossa
            tabname   = 'COSSA'
          TABLES
            db_table  = lt_cossa
          EXCEPTIONS
            OTHERS    = 0.
        LOOP AT lt_cossa.
          MOVE-CORRESPONDING lt_cossa TO gs_covja_ext.
          APPEND gs_covja_ext TO gt_covja_ext.
        ENDLOOP.
*.....collect master keys into global tables
        MOVE-CORRESPONDING gs_covja_ext TO ls_covj.
        PERFORM collect_master_keys_covj USING ls_covj.
        PERFORM gui_counter TABLES gt_covja_ext
                            CHANGING ld_exit.
        IF NOT ld_exit IS INITIAL.
          EXIT.
        ENDIF.
      ENDSELECT.
      IF NOT ld_exit IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
    IF NOT ld_exit IS INITIAL OR NOT ld_exit_sel IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                               " SELECT_COVJ

*---------------------------------------------------------------------*
FORM select_cobk_coep
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
USING VALUE(v_callback_programm) LIKE sy-repid
      VALUE(vt_var_cond_pos)     TYPE kaep_t_var_cond
      VALUE(vt_var_cond_head)    TYPE kaep_t_var_cond
      VALUE(vt_fields_pos)       TYPE kaep_t_fieldname
      VALUE(vt_fields_head)      TYPE kaep_t_fieldname
      VALUE(v_pos_table)         TYPE kaep_tabname
      VALUE(v_head_table)        TYPE kaep_tabname.
*----------------------------------------------------------------------*

  DATA: ls_covp LIKE covp,
        ld_exit TYPE kaep_flag.

* read document (head)
  SELECT (vt_fields_head)
         APPENDING CORRESPONDING FIELDS OF TABLE gt_cobk_ext
         FROM (v_head_table) WHERE kokrs = gd-kokrs
                             AND   belnr IN gr_belnr
                             AND   (vt_var_cond_head).

  CHECK NOT gt_cobk_ext[] IS INITIAL.

  LOOP AT gt_cobk_ext INTO gs_cobk_ext.
    SELECT (vt_fields_pos) INTO CORRESPONDING FIELDS OF gs_coep_ext
        FROM (v_pos_table) WHERE kokrs = gs_cobk_ext-kokrs
                           AND   belnr = gs_cobk_ext-belnr
                           AND   versn IN gr_versn
                           AND   wrttp IN gr_wrttp
                           AND  (vt_var_cond_pos)
                           ORDER BY PRIMARY KEY.
      APPEND gs_coep_ext TO gt_coep_ext.
*.....collect master keys into global tables
      MOVE-CORRESPONDING gs_coep_ext TO ls_covp.
      PERFORM collect_master_keys_covp USING ls_covp.
      PERFORM gui_counter TABLES gt_coep_ext
                          CHANGING ld_exit.
      IF NOT ld_exit IS INITIAL.
        EXIT.
      ENDIF.
    ENDSELECT.
  ENDLOOP.

ENDFORM.                               " SELECT_COBK_COEP

*---------------------------------------------------------------------*
FORM select_cobk_coej
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
USING VALUE(v_callback_programm) LIKE sy-repid
      VALUE(vt_var_cond_pos)     TYPE kaep_t_var_cond
      VALUE(vt_var_cond_head)    TYPE kaep_t_var_cond
      VALUE(vt_fields_pos)       TYPE kaep_t_fieldname
      VALUE(vt_fields_head)      TYPE kaep_t_fieldname
      VALUE(v_pos_table)         TYPE kaep_tabname
      VALUE(v_head_table)        TYPE kaep_tabname.
*----------------------------------------------------------------------*

  DATA: ls_covj LIKE covj,
        ld_exit TYPE kaep_flag.

  IF generated_periods NE con_percl.
    MESSAGE x499(kb) WITH 'SELECT_COBK_COEJ(SAPLKAEP)'.
  ENDIF.

* read document (head)
  SELECT (vt_fields_head)
         APPENDING CORRESPONDING FIELDS OF TABLE gt_cobk_ext
         FROM (v_head_table) WHERE kokrs = gd-kokrs
                             AND   belnr IN gr_belnr
                             AND   (vt_var_cond_head).

  CHECK NOT gt_cobk_ext[] IS INITIAL.

  LOOP AT gt_cobk_ext INTO gs_cobk_ext.
    SELECT (vt_fields_pos) INTO CORRESPONDING FIELDS OF gs_coeja_ext
         FROM (v_pos_table) WHERE kokrs = gs_cobk_ext-kokrs
                            AND   belnr = gs_cobk_ext-belnr
                            AND  (vt_var_cond_pos)
                            ORDER BY PRIMARY KEY.
      APPEND gs_coeja_ext TO gt_coeja_ext.
*.....collect master keys into global tables
      MOVE-CORRESPONDING gs_coeja_ext TO ls_covj.
      PERFORM collect_master_keys_covj USING ls_covj.
      PERFORM gui_counter TABLES gt_coeja_ext
                          CHANGING ld_exit.
      IF NOT ld_exit IS INITIAL.
        EXIT.
      ENDIF.
    ENDSELECT.
  ENDLOOP.

ENDFORM.                               " SELECT_COBK_COEJ

*&---------------------------------------------------------------------*
FORM select_cooi
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
USING VALUE(v_callback_programm) LIKE sy-repid
      VALUE(vt_var_cond)         TYPE kaep_t_var_cond
      VALUE(vt_fields)           TYPE kaep_t_fieldname
      VALUE(v_db_table)          TYPE kaep_tabname.
*----------------------------------------------------------------------*

  DATA: ls_cooi       LIKE cooi,
        lr_objnr      LIKE gr_objnr OCCURS 0,
        ld_sel_lines  LIKE sy-tfill VALUE 0,
        ld_obj_lines  LIKE sy-tfill VALUE 0,
        ld_bloc_lines LIKE sy-tfill VALUE 0,
        ld_bloc_tab   TYPE kaep_fieldname,
        ld_blocs      LIKE sy-tfill VALUE 0,
        ld_exit       TYPE kaep_flag,
        ld_exit_sel   TYPE kaep_flag,
        lr_kstar      LIKE gr_kstar OCCURS 0.


  "{ Begin ENHO GPD_SAPLKAEP IS-AD-GPD AD_GPD }
* 2011587 ->
  DATA: "lt_dis_cosp  TYPE SORTED TABLE OF dis_tobjs_cosp WITH NON-UNIQUE KEY objnr,      "2198892
    lt_cooi_ext  TYPE SORTED TABLE OF kaep_cooi_ext  WITH NON-UNIQUE KEY refbt refbn rfpos gjahr perio,
    lt_cooi_ext2 TYPE SORTED TABLE OF kaep_cooi_ext  WITH NON-UNIQUE KEY refbt objnr gjahr perio, "2257465
*        lt_grpnr_rng LIKE RANGE OF dis_tobjs_cosp-grpnr,                 "2198892 ->
    lv_grpnr     TYPE dis_tobjs_cosp-grpnr,
    lt_dis_cosp  TYPE SORTED   TABLE OF dis_tobjs_cosp WITH NON-UNIQUE KEY mandt grpnr ass_objnr objnr gjahr perio wrttp kstar hrkft vrgng beknz twaer perbl,
    lt_grpnr     TYPE STANDARD TABLE OF dis_tobjs_cosp-grpnr,
    lt_ass_objnr TYPE STANDARD TABLE OF dis_tobjs_cosp-ass_objnr, "2198892 <-
    lv_refbt     TYPE cooi-refbt,
    lv_refbn     TYPE cooi-refbn,
    lv_rfpos     TYPE cooi-rfpos.

  FIELD-SYMBOLS: "<grpnr_rng>  LIKE LINE OF lt_grpnr_rng,                 "2198892
    <objnr>      LIKE LINE OF lr_objnr,
    <dis_cosp>   LIKE LINE OF lt_dis_cosp,
    <cooi_ext>   LIKE LINE OF gt_cooi_ext,
    <cooi_ext_n> LIKE LINE OF gt_cooi_ext.
* 2011587 <-
  "{ End ENHO GPD_SAPLKAEP IS-AD-GPD AD_GPD }


ENHANCEMENT-POINT lkaepfsl_select_cooi_01 SPOTS es_saplkaep STATIC .

*.new KAEPX functionality
  IF NOT gt_kaepx_data_carrier[] IS INITIAL.
    READ TABLE gt_kaepx_data_carrier INTO
               gs_kaepx_data_carrier INDEX 1.
*....check if KAEPX-relevant data is handed over
    MOVE-CORRESPONDING gs_kaepx_data_carrier TO
         gs_kaepx_relevant.
    IF NOT gs_kaepx_relevant IS INITIAL.
      PERFORM select_cooi_kaepx USING vt_var_cond
                                      vt_fields
                                      v_db_table.
      EXIT.
    ENDIF.
  ENDIF.

  PERFORM add_lines_count TABLES   gr_gjahr
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_perio
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_versn
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_wrttp
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_budat
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_wkgbtr
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   vt_var_cond
                          CHANGING ld_sel_lines.

  IF ld_sel_lines >= con_max_sel_lines.
    ld_bloc_lines = 1.
  ELSE.
    ld_bloc_lines = con_max_sel_lines - ld_sel_lines.
  ENDIF.

  DESCRIBE TABLE gr_objnr LINES ld_obj_lines.
  DESCRIBE TABLE gr_kstar LINES sy-tfill.

* bloc larger table
  IF ld_obj_lines > sy-tfill.
    ld_bloc_tab = con_field-objnr.
    sy-tfill = ld_obj_lines.
  ELSE.
    ld_bloc_tab = con_field-kstar.
  ENDIF.

  IF sy-tfill > ld_bloc_lines.
    PERFORM clc_blocs USING    sy-tfill
                               ld_bloc_lines
                      CHANGING ld_blocs.
  ELSE.
    ld_blocs = 1.
  ENDIF.

  DO.

    CASE ld_bloc_tab.
      WHEN con_field-kstar.
        REFRESH lr_objnr.
        READ TABLE gr_objnr INDEX sy-index.
        IF sy-subrc <> 0.
*         no select without object: exit immediately
          EXIT.
        ELSE.
          APPEND gr_objnr TO lr_objnr.
        ENDIF.
      WHEN con_field-objnr.
        REFRESH lr_kstar.
        IF gr_kstar[] IS INITIAL.
*         one select without cost element possible: exit after select
          ld_exit_sel = 'X'.
        ELSE.
          READ TABLE gr_kstar INDEX sy-index.
          IF sy-subrc <> 0.
            EXIT.
          ELSE.
            APPEND gr_kstar TO lr_kstar.
          ENDIF.
        ENDIF.
    ENDCASE.

    DO ld_blocs TIMES.
      CASE ld_bloc_tab.
        WHEN con_field-kstar.
          PERFORM fill_bloc TABLES lr_kstar
                                   gr_kstar
                            USING  sy-index
                                   ld_bloc_lines.
        WHEN con_field-objnr.
          PERFORM fill_bloc TABLES lr_objnr
                                   gr_objnr
                            USING  sy-index
                                   ld_bloc_lines.
      ENDCASE.

      SELECT (vt_fields) INTO CORRESPONDING FIELDS OF gs_cooi_ext
                         FROM (v_db_table)
                         WHERE lednr = '00'
                         AND   objnr  IN lr_objnr
                         AND   gjahr  IN gr_gjahr
                         AND   versn  IN gr_versn
                         AND   wrttp  IN gr_wrttp
                         AND   sakto  IN lr_kstar
                         AND   budat  IN gr_budat
                         AND   perio  IN gr_perio
                         AND   wkgbtr IN gr_wkgbtr
                         AND   (vt_var_cond).
        CHECK gs_cooi_ext-kokrs = gd-kokrs.
        APPEND gs_cooi_ext TO gt_cooi_ext.
        MOVE-CORRESPONDING gs_cooi_ext TO ls_cooi.
        PERFORM collect_master_keys_cooi USING ls_cooi.
        PERFORM gui_counter TABLES gt_cooi_ext
                            CHANGING ld_exit.
        IF NOT ld_exit IS INITIAL.
          EXIT.
        ENDIF.
      ENDSELECT.
      IF NOT ld_exit IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
    IF NOT ld_exit IS INITIAL OR NOT ld_exit_sel IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.



  "{ Begin ENHO GPD_SAPLKAEP IS-AD-GPD AD_GPD }
* 2011587 ->
  IF gd-rep_object NE objektart_pd OR gr_objnr IS INITIAL.        "2193883    "2198892
    RETURN.
  ENDIF.

* 2198892 ->
*  LOOP AT lr_objnr ASSIGNING <objnr> WHERE low+0(2) EQ 'PR'.
*    APPEND INITIAL LINE TO lt_grpnr_rng ASSIGNING <grpnr_rng>.
*    <grpnr_rng>-sign   = <objnr>-sign.
*    <grpnr_rng>-option = <objnr>-option.
*    <grpnr_rng>-low    = <objnr>-low.
*    <grpnr_rng>-high   = <objnr>-high.
*  ENDLOOP.
*
*  SELECT *
*    FROM dis_tobjs_cosp
*    INTO TABLE lt_dis_cosp
*    WHERE ( grpnr     IN lt_grpnr_rng OR
*            ass_objnr IN lr_objnr ) AND
*          gjahr IN gr_gjahr         AND
*          wrttp IN gr_wrttp         AND
*          kstar IN lr_kstar         AND
*          perio IN gr_perio         AND
*          wkg   IN gr_wkgbtr        AND
*          vrgng EQ 'GPDO'.

  LOOP AT gr_objnr ASSIGNING <objnr> WHERE low+0(2) EQ 'PR'.
    lv_grpnr = <objnr>-low+2(8).
    COLLECT lv_grpnr    INTO lt_grpnr.
    COLLECT <objnr>-low INTO lt_ass_objnr.
  ENDLOOP.

  IF lt_grpnr IS INITIAL.
    RETURN.
  ENDIF.

  SELECT *
    FROM dis_tobjs_cosp
    INTO TABLE lt_dis_cosp
    FOR ALL ENTRIES IN lt_grpnr
    WHERE grpnr EQ lt_grpnr-table_line AND
          gjahr IN gr_gjahr            AND
          wrttp IN gr_wrttp            AND
*         kstar IN lr_kstar            AND                          "2404396
          kstar IN gr_kstar            AND                  "2404396
          perio IN gr_perio            AND
          wkg   IN gr_wkgbtr           AND
          vrgng EQ 'GPDO'.

  SELECT *
    FROM dis_tobjs_cosp
    APPENDING TABLE lt_dis_cosp
    FOR ALL ENTRIES IN lt_ass_objnr
    WHERE ass_objnr EQ lt_ass_objnr-table_line AND
          gjahr IN gr_gjahr         AND
          wrttp IN gr_wrttp         AND
*         kstar     IN lr_kstar                AND                  "2404396
          kstar     IN gr_kstar                AND          "2404396
          perio IN gr_perio         AND
          wkg   IN gr_wkgbtr        AND
          vrgng EQ 'GPDO'.

  DELETE ADJACENT DUPLICATES FROM lt_dis_cosp.
* 2198892 <-

  lt_cooi_ext = gt_cooi_ext.
  lt_cooi_ext2 = gt_cooi_ext.                               "2257465

  LOOP AT lt_dis_cosp ASSIGNING <dis_cosp>.

    IF <dis_cosp>-objnr(2) EQ 'BA' OR <dis_cosp>-objnr(2) EQ 'BE'. "2257465

      IF <dis_cosp>-objnr+0(2) EQ 'BA'.
        lv_refbt = '010'. "Preq
      ELSE.
        lv_refbt = '020'. "PO
      ENDIF.
      lv_refbn = <dis_cosp>-objnr+2(10).
      lv_rfpos = <dis_cosp>-objnr+13(5).

      UNASSIGN: <cooi_ext>.

      READ TABLE lt_cooi_ext WITH       KEY refbt = lv_refbt
                                          refbn = lv_refbn
                                          rfpos = lv_rfpos
*                                            gjahr = <dis_cosp>-gjahr   "2265793
*                                            perio = <dis_cosp>-perio   "2265793
                             ASSIGNING <cooi_ext>.

    ELSEIF <dis_cosp>-objnr(2) EQ 'OR'.                     "2257465 ->

      UNASSIGN: <cooi_ext>.
      READ TABLE lt_cooi_ext2 WITH       KEY refbt = '020'                "Read PPO 1st prio
                                             objnr = <dis_cosp>-objnr
*                                             gjahr = <dis_cosp>-gjahr  "2265793
*                                             perio = <dis_cosp>-perio  "2265793
                              ASSIGNING <cooi_ext>.
      IF sy-subrc IS NOT INITIAL.
        READ TABLE lt_cooi_ext2 WITH       KEY refbt = '010'              "Read PReq 2nd prio
                                               objnr = <dis_cosp>-objnr
*                                               gjahr = <dis_cosp>-gjahr  "2265793
*                                               perio = <dis_cosp>-perio  "2265793
                                ASSIGNING <cooi_ext>.
      ENDIF.

    ELSE.
      CONTINUE.
    ENDIF.                                                  "2257465 <-

*   --- Core commitment found -> create grouping WBS element credit ---
    IF sy-subrc IS INITIAL.

      APPEND INITIAL LINE TO gt_cooi_ext ASSIGNING <cooi_ext_n>.
      MOVE-CORRESPONDING: <cooi_ext> TO <cooi_ext_n>,
                          <dis_cosp> TO <cooi_ext_n>.

      CONCATENATE 'PR' <dis_cosp>-grpnr INTO <cooi_ext_n>-objnr.
      <cooi_ext_n>-rftrm  = '00001'.
      <cooi_ext_n>-beknz  = 'H'.
      <cooi_ext_n>-sakto  = <dis_cosp>-kstar.
      <cooi_ext_n>-megbtr = <dis_cosp>-meg * -1.
      <cooi_ext_n>-wkgbtr = <dis_cosp>-wkg * -1.
      <cooi_ext_n>-wtgbtr = <dis_cosp>-wtg * -1.
      <cooi_ext_n>-wogbtr = <dis_cosp>-wog * -1.

      IF <dis_cosp>-objnr(2) EQ 'OR'.                       "2257465 ->
        CLEAR: <cooi_ext_n>-refbt, <cooi_ext_n>-refbn, <cooi_ext_n>-rfpos.
        <cooi_ext_n>-aufnr = <dis_cosp>-objnr+2.            "2265794
      ENDIF.                                                "2257465 <-

    ENDIF.

*   --- Create assigned WBS element debit posting ---

    IF <cooi_ext> IS ASSIGNED.
*     --- Re-use core posting data if available
      APPEND INITIAL LINE TO gt_cooi_ext ASSIGNING <cooi_ext_n>.
      MOVE-CORRESPONDING <cooi_ext> TO <cooi_ext_n>.
    ELSE.
*     --- Identify core posting data
      IF <dis_cosp>-objnr(2) EQ 'BA' OR <dis_cosp>-objnr(2) EQ 'BE'. "2257465
        IF <dis_cosp>-objnr+0(2) EQ 'BA'.
          lv_refbt = '010'. "Preq
        ELSE.
          lv_refbt = '020'. "PO
        ENDIF.
        lv_refbn = <dis_cosp>-objnr+2(10).
        lv_rfpos = <dis_cosp>-objnr+13(5).

        SELECT SINGLE *
          FROM cooi
          INTO CORRESPONDING FIELDS OF gs_cooi_ext
          WHERE refbt EQ lv_refbt         AND
                refbn EQ lv_refbn         AND
                rfpos EQ lv_rfpos         AND
*                gjahr EQ <dis_cosp>-gjahr AND                      "2265793 ->
*                perio EQ <dis_cosp>-perio.
                  gjahr IN gr_gjahr         AND
                  perio IN gr_perio.   "#EC_WARNOK                    "2265793 <-

      ELSEIF <dis_cosp>-objnr(2) EQ 'OR'.                   "2257465 ->

        SELECT SINGLE *                                   "Read PPO 1st prio
          FROM cooi
          INTO CORRESPONDING FIELDS OF gs_cooi_ext
          WHERE refbt EQ '020'            AND
                objnr EQ <dis_cosp>-objnr AND
*                gjahr EQ <dis_cosp>-gjahr AND                      "2265793 ->
*                perio EQ <dis_cosp>-perio.
                gjahr IN gr_gjahr         AND
                perio IN gr_perio.   "#EC_WARNOK                    "2265793 <-

        IF sy-subrc IS NOT INITIAL.
          SELECT SINGLE *                                 "Read PReq 2nd prio
            FROM cooi
            INTO CORRESPONDING FIELDS OF gs_cooi_ext
            WHERE refbt EQ '010'            AND
                  objnr EQ <dis_cosp>-objnr AND
*                gjahr EQ <dis_cosp>-gjahr AND                      "2265793 ->
*                perio EQ <dis_cosp>-perio.
                gjahr IN gr_gjahr         AND
                perio IN gr_perio.   "#EC_WARNOK                    "2265793 <-
        ENDIF.

      ENDIF.                                                "2257465 <-

      CHECK sy-subrc IS INITIAL AND gs_cooi_ext-budat IN gr_budat. "2257465

      APPEND INITIAL LINE TO gt_cooi_ext ASSIGNING <cooi_ext_n>.
      MOVE-CORRESPONDING gs_cooi_ext TO <cooi_ext_n>.
    ENDIF.

    MOVE-CORRESPONDING <dis_cosp> TO <cooi_ext_n>.

    IF <dis_cosp>-objnr(2) EQ 'OR'.                         "2257465 ->
      CLEAR: <cooi_ext_n>-refbt, <cooi_ext_n>-refbn, <cooi_ext_n>-rfpos.
      <cooi_ext_n>-aufnr = <dis_cosp>-objnr+2.              "2265794
    ENDIF.                                                  "2257465 <-

    <cooi_ext_n>-objnr  = <dis_cosp>-ass_objnr.
    <cooi_ext_n>-rftrm  = '00001'.
    <cooi_ext_n>-beknz  = 'S'.
    <cooi_ext_n>-sakto  = <dis_cosp>-kstar.
    <cooi_ext_n>-megbtr = <dis_cosp>-meg.
    <cooi_ext_n>-wkgbtr = <dis_cosp>-wkg.
    <cooi_ext_n>-wtgbtr = <dis_cosp>-wtg.
    <cooi_ext_n>-wogbtr = <dis_cosp>-wog.

*   --- Store assigned WBS in master keys (grouping WBS is already there from COOI)
    MOVE-CORRESPONDING <cooi_ext_n> TO ls_cooi.
    PERFORM collect_master_keys_cooi USING ls_cooi.

*   --- Grouping WBS does not come from COOI in some cases          "2404396 ->
    CLEAR ls_cooi.
    CONCATENATE 'PR' <dis_cosp>-grpnr INTO ls_cooi-objnr.
    PERFORM collect_master_keys_cooi USING ls_cooi.         "2404396 <-

*   --- Exit out if maximum linecount reached
    PERFORM gui_counter TABLES gt_cooi_ext
                        CHANGING ld_exit.
    IF ld_exit IS NOT INITIAL.
      EXIT.
    ENDIF.

  ENDLOOP.
* 2011587 <-
  "{ End ENHO GPD_SAPLKAEP IS-AD-GPD AD_GPD }


ENHANCEMENT-POINT lkaepfsl_select_cooi_02 SPOTS es_saplkaep .

ENDFORM.                               " SELECT_COOI

*&---------------------------------------------------------------------*
FORM select_bpvj
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
USING VALUE(v_callback_programm) LIKE sy-repid
      VALUE(vt_var_cond)         TYPE kaep_t_var_cond
      VALUE(vt_fields)           TYPE kaep_t_fieldname
      VALUE(v_db_table)          TYPE kaep_tabname.
*----------------------------------------------------------------------*
  TABLES: bpvg.
  DATA: ld_exit       TYPE kaep_flag,
        ld_sel_lines  LIKE sy-tfill VALUE 0,
        ld_bloc_lines LIKE sy-tfill VALUE 0,
        ld_blocs      LIKE sy-tfill VALUE 0,
        ls_bpvg       LIKE bpvg,
        ls_fields_ges TYPE kaep_fieldname,
        lt_fields_ges TYPE kaep_t_fieldname,
        lr_objnr_bloc LIKE gr_objnr OCCURS 0.

  DATA: BEGIN OF ls_bpvg_ext,
          bpvj_ext TYPE kaep_bpvj_ext,
          wtges    LIKE bpge-wtges,
          wlges    LIKE bpge-wlges,
        END OF ls_bpvg_ext.

* fill ls_fields_ges if total values have to be selected
  IF NOT gd_flgjg IS INITIAL.
    LOOP AT vt_fields INTO ls_fields_ges.
      IF     ls_fields_ges = 'WTJHR'.
        ls_fields_ges = 'WTGES'.
      ELSEIF ls_fields_ges = 'WLJHR'.
        ls_fields_ges = 'WLGES'.
      ENDIF.
* field 'SPRED' is not in 'BPVG' and hence it must not be selected
      IF NOT ls_fields_ges = 'SPRED'.
        APPEND ls_fields_ges TO lt_fields_ges.
      ENDIF.
    ENDLOOP.
  ENDIF.

  PERFORM add_lines_count TABLES   gr_bldat
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_wrttp
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_gjahr
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   vt_var_cond
                          CHANGING ld_sel_lines.

  IF ld_sel_lines >= con_max_sel_lines.
    ld_bloc_lines = 1.
  ELSE.
    ld_bloc_lines = con_max_sel_lines - ld_sel_lines.
  ENDIF.
  DESCRIBE TABLE gr_objnr LINES sy-tfill.
  IF sy-tfill > ld_bloc_lines.
    PERFORM clc_blocs USING    sy-tfill
                               ld_bloc_lines
                      CHANGING ld_blocs.
  ELSE.
    ld_blocs = 1.
  ENDIF.
* block objects here since no cost elements to be selected
  DO ld_blocs TIMES.
    PERFORM fill_bloc TABLES lr_objnr_bloc
                             gr_objnr
                      USING  sy-index
                             ld_bloc_lines.
    IF lr_objnr_bloc[] IS INITIAL.
*     do not process without objetcs!
      EXIT.
    ENDIF.
    CLEAR gs_bpvj_ext.
    IF NOT gr_gjahr[] IS INITIAL.
      SELECT (vt_fields) INTO CORRESPONDING FIELDS OF gs_bpvj_ext
                         FROM (v_db_table)
                         WHERE lednr IN gr_lednr
                         AND   objnr IN lr_objnr_bloc
                         AND   gjahr IN gr_gjahr
                         AND   wrttp IN gr_wrttp
                         AND   bldat IN gr_bldat
                         AND   versn IN gr_versn
                         AND   (vt_var_cond).
        gs_bpvj_ext-kokrs = gd-kokrs.
        APPEND gs_bpvj_ext TO gt_bpvj_ext.
        IF NOT gs_bpvj_ext-objnr IS INITIAL.
          COLLECT gs_bpvj_ext-objnr    INTO gt_objnr.
        ENDIF.
        PERFORM gui_counter TABLES gt_bpvj_ext
                            CHANGING ld_exit.
        IF NOT ld_exit IS INITIAL.
          EXIT.
        ENDIF.
      ENDSELECT.
    ENDIF.
    IF NOT gd_flgjg IS INITIAL.
      SELECT (lt_fields_ges) INTO CORRESPONDING FIELDS OF ls_bpvg
                             FROM  bpvg
                             WHERE lednr IN gr_lednr
                             AND   objnr IN lr_objnr_bloc
                             AND   wrttp IN gr_wrttp
                             AND   bldat IN gr_bldat
                             AND   versn IN gr_versn
                             AND   (vt_var_cond).
        MOVE-CORRESPONDING ls_bpvg TO gs_bpvj_ext.
        gs_bpvj_ext-kokrs = gd-kokrs.
        gs_bpvj_ext-wtjhr = ls_bpvg-wtges.
        gs_bpvj_ext-wljhr = ls_bpvg-wlges.
        gs_bpvj_ext-flgjg = 'X'.
        APPEND gs_bpvj_ext TO gt_bpvj_ext.
        IF NOT gs_bpvj_ext-objnr IS INITIAL.
          COLLECT gs_bpvj_ext-objnr    INTO gt_objnr.
        ENDIF.
        PERFORM gui_counter TABLES gt_bpvj_ext
                            CHANGING ld_exit.
        IF NOT ld_exit IS INITIAL.
          EXIT.
        ENDIF.
      ENDSELECT.
    ENDIF.
    IF NOT ld_exit IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.
* if line item is called from report report interface only totals or
* annual values are to be displayed
  IF NOT gd-called_by_rri IS INITIAL.
    LOOP AT gt_bpvj_ext TRANSPORTING NO FIELDS
                        WHERE flgjg = 'X'
                        AND   wljhr <> 0.
      EXIT.
    ENDLOOP.
    IF sy-subrc IS INITIAL.
*     delete all annual values
      DELETE gt_bpvj_ext WHERE NOT flgjg = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.                               " SELECT_BPVJ

*---------------------------------------------------------------------*
*       FORM select_bpvp                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  value(v_callback_programm)                                    *
*  -->  value(vt_var_cond)                                            *
*  -->  value(vt_fields)                                              *
*  -->  value(v_db_table)                                             *
*---------------------------------------------------------------------*
FORM select_bpvp
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
USING VALUE(v_callback_programm) LIKE sy-repid
      VALUE(vt_var_cond)         TYPE kaep_t_var_cond
      VALUE(vt_fields)           TYPE kaep_t_fieldname
      VALUE(v_db_table)          TYPE kaep_tabname.
*----------------------------------------------------------------------*
  TABLES: bpvp.
  DATA: ld_exit       TYPE kaep_flag,
        ld_sel_lines  LIKE sy-tfill VALUE 0,
        ld_bloc_lines LIKE sy-tfill VALUE 0,
        ld_blocs      LIKE sy-tfill VALUE 0,
        ls_bpvg       LIKE bpvg,
        ls_fields_ges TYPE kaep_fieldname,
        lt_fields_ges TYPE kaep_t_fieldname,
        lr_objnr_bloc LIKE gr_objnr OCCURS 0.

  PERFORM add_lines_count TABLES   gr_bldat
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_wrttp
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_gjahr
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_perio
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   vt_var_cond
                          CHANGING ld_sel_lines.

  IF ld_sel_lines >= con_max_sel_lines.
    ld_bloc_lines = 1.
  ELSE.
    ld_bloc_lines = con_max_sel_lines - ld_sel_lines.
  ENDIF.
  DESCRIBE TABLE gr_objnr LINES sy-tfill.
  IF sy-tfill > ld_bloc_lines.
    PERFORM clc_blocs USING    sy-tfill
                               ld_bloc_lines
                      CHANGING ld_blocs.
  ELSE.
    ld_blocs = 1.
  ENDIF.
* block objects here since no cost elements to be selected
  DO ld_blocs TIMES.
    PERFORM fill_bloc TABLES lr_objnr_bloc
                             gr_objnr
                      USING  sy-index
                             ld_bloc_lines.
    CLEAR gs_bpvp_ext.
    SELECT (vt_fields) INTO CORRESPONDING FIELDS OF gs_bpvp_ext
                         FROM (v_db_table)
                         WHERE lednr IN gr_lednr
                         AND   objnr IN lr_objnr_bloc
                         AND   wrttp IN gr_wrttp
                         AND   gjahr IN gr_gjahr
                         AND   bldat IN gr_bldat
                         AND   (vt_var_cond).
      gs_bpvp_ext-kokrs = gd-kokrs.
      APPEND gs_bpvp_ext TO gt_bpvp_ext.
      IF NOT gs_bpvp_ext-objnr IS INITIAL.
        COLLECT gs_bpvp_ext-objnr    INTO gt_objnr.
      ENDIF.
      PERFORM gui_counter TABLES gt_bpvp_ext
                          CHANGING ld_exit.
      IF NOT ld_exit IS INITIAL.
        EXIT.
      ENDIF.
    ENDSELECT.
    IF NOT ld_exit IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                               " SELECT_BPVJ


*&---------------------------------------------------------------------*
FORM select_v_cofp
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
USING VALUE(v_callback_programm) LIKE sy-repid
      VALUE(vt_var_cond)         TYPE kaep_t_var_cond
      VALUE(vt_fields)           TYPE kaep_t_fieldname
      VALUE(v_db_table)          TYPE kaep_tabname.
*----------------------------------------------------------------------*

  DATA: ld_exit       TYPE kaep_flag,
        ld_sel_lines  LIKE sy-tfill VALUE 0,
        ld_bloc_lines LIKE sy-tfill VALUE 0,
        ld_blocs      LIKE sy-tfill VALUE 0,
        ls_t001       LIKE t001,
        ls_cofp       LIKE cofp,
        lr_objnr_bloc LIKE gr_objnr OCCURS 0.

* POSIT will be needed in VT_FIELDS later to read FIPOS  "2629849
  READ TABLE vt_fields WITH KEY table_line = 'POSIT'        "2629849
                       TRANSPORTING NO FIELDS.              "2629849
  IF sy-subrc IS NOT INITIAL.                               "2629849
    APPEND 'POSIT' TO vt_fields.                            "2629849
  ENDIF.                                                    "2629849

  PERFORM add_lines_count TABLES   gr_budat
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_wrttp
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_perio
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   gr_gjahr
                          CHANGING ld_sel_lines.
  PERFORM add_lines_count TABLES   vt_var_cond
                          CHANGING ld_sel_lines.

  IF ld_sel_lines >= con_max_sel_lines.
    ld_bloc_lines = 1.
  ELSE.
    ld_bloc_lines = con_max_sel_lines - ld_sel_lines.
  ENDIF.
  DESCRIBE TABLE gr_objnr LINES sy-tfill.
  IF sy-tfill > ld_bloc_lines.
    PERFORM clc_blocs USING    sy-tfill
                               ld_bloc_lines
                      CHANGING ld_blocs.
  ELSE.
    ld_blocs = 1.
  ENDIF.
* block objects here since no cost elements to be selected
  DO ld_blocs TIMES.
    PERFORM fill_bloc TABLES lr_objnr_bloc
                             gr_objnr
                      USING  sy-index
                             ld_bloc_lines.
    SELECT (vt_fields) INTO CORRESPONDING FIELDS OF gs_cofp_ext
                       FROM (v_db_table)
                       WHERE objnr IN lr_objnr_bloc
                       AND   perio IN gr_perio
                       AND   gjahr IN gr_gjahr
                       AND   wrttp IN gr_wrttp
                       AND   zhldt IN gr_budat
                       AND   (vt_var_cond).
      CHECK gs_cofp_ext-kokrs = gd-kokrs.
      APPEND gs_cofp_ext TO gt_cofp_ext.
      IF NOT gs_cofp_ext-objnr IS INITIAL.
        COLLECT gs_cofp_ext-objnr INTO gt_objnr.
      ENDIF.
      PERFORM gui_counter TABLES   gt_cofp_ext
                          CHANGING ld_exit.
      IF NOT ld_exit IS INITIAL.
        EXIT.
      ENDIF.
    ENDSELECT.
  ENDDO.
  LOOP AT gt_cofp_ext INTO gs_cofp_ext.
    IF ls_t001-bukrs NE gs_cofp_ext-bukrs.
      SELECT SINGLE * FROM t001 INTO  ls_t001
                                WHERE bukrs = gs_cofp_ext-bukrs.
    ENDIF.
    CALL FUNCTION 'GET_FIPOS_FROM_POSIT'
      EXPORTING
        ip_fikrs     = ls_t001-fikrs
        ip_fma_objnr = gs_cofp_ext-objnr
        ip_posit     = gs_cofp_ext-posit
      IMPORTING
*       OP_FIKRS     =
        op_fipos     = gs_cofp_ext-fipos
      EXCEPTIONS
        not_found    = 1
        OTHERS       = 2.

    IF sy-subrc = 0.
      MODIFY gt_cofp_ext FROM gs_cofp_ext.
    ENDIF.
  ENDLOOP.
ENDFORM.                               " SELECT_V_COFP
*&---------------------------------------------------------------------*
FORM read_master_data
*&---------------------------------------------------------------------*
USING VALUE(v_text_flags) TYPE kaep_text_flags
      VALUE(v_item_group) TYPE kaep_item_group
      VALUE(v_rep_object) TYPE kaep_rep_object
      VALUE(v_usear)      TYPE kaep_flag
      VALUE(vt_selection) TYPE kaep_t_cosel.
*&---------------------------------------------------------------------*
  PERFORM read_master_controlling_area USING v_usear.
  CHECK: gd_kaepx_active <> 'X'.
*..read the objects and their texts
  IF NOT gt_objnr[] IS INITIAL.

    CALL FUNCTION 'K_PERFORMANCE_START'
      EXPORTING
        i_tabname = con_table-onr00.

*...delete read objects from key table
    LOOP AT gt_objnr.
      READ TABLE gt_objnr_master WITH KEY objnr = gt_objnr
                                 TRANSPORTING NO FIELDS
                                 BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE gt_objnr.
      ENDIF.
    ENDLOOP.

    PERFORM read_master_objects USING v_text_flags-object
                                      v_text_flags-object_group
                                      v_item_group
                                      v_rep_object
                                      v_usear
                                      vt_selection.
    DESCRIBE TABLE gt_objnr LINES sy-tfill.
    CALL FUNCTION 'K_PERFORMANCE_STOP'
      EXPORTING
        i_tabname        = con_table-onr00
        i_record_counter = sy-tfill.

  ENDIF.

*..read the cost element texts
  IF NOT gt_kstar[] IS INITIAL AND
    ( NOT v_text_flags-cost_element IS INITIAL OR
      NOT v_text_flags-cost_element_group IS INITIAL ).

    CALL FUNCTION 'K_PERFORMANCE_START'
      EXPORTING
        i_tabname = con_table-csku.

*...delete read cost elements from key table
    LOOP AT gt_kstar.
      READ TABLE gt_kstar_master WITH KEY kstar = gt_kstar
                          TRANSPORTING NO FIELDS
                          BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE gt_kstar.
      ENDIF.
    ENDLOOP.

    PERFORM read_master_cost_elements
                           USING v_text_flags-cost_element_group.
    DESCRIBE TABLE gt_kstar LINES sy-tfill.

    CALL FUNCTION 'K_PERFORMANCE_STOP'
      EXPORTING
        i_tabname        = con_table-csku
        i_record_counter = sy-tfill.

  ENDIF.

*..read qmnum text
  IF NOT gt_qmnum[] IS INITIAL AND
     NOT v_text_flags-qmnum IS INITIAL.

    CALL FUNCTION 'K_PERFORMANCE_START'
      EXPORTING
        i_tabname = con_table-qmel.

*...delete read qmnum from key table
    LOOP AT gt_qmnum.
      READ TABLE gt_qmnum_master WITH KEY qmnum = gt_qmnum
                          TRANSPORTING NO FIELDS
                          BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE gt_qmnum.
      ENDIF.
    ENDLOOP.

    PERFORM read_master_qmnum.

    DESCRIBE TABLE gt_qmnum LINES sy-tfill.

    CALL FUNCTION 'K_PERFORMANCE_STOP'
      EXPORTING
        i_tabname        = con_table-qmel
        i_record_counter = sy-tfill.

  ENDIF.

*..read the material texts
  IF NOT gt_matnr[] IS INITIAL AND
     NOT v_text_flags-material IS INITIAL.

    CALL FUNCTION 'K_PERFORMANCE_START'
      EXPORTING
        i_tabname = con_table-makt.

*...delete read materials from key table
    LOOP AT gt_matnr.
      READ TABLE gt_matnr_master WITH KEY matnr = gt_matnr
                                 TRANSPORTING NO FIELDS
                                 BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE gt_matnr.
      ENDIF.
    ENDLOOP.

    PERFORM read_master_material.

    DESCRIBE TABLE gt_matnr LINES sy-tfill.
    CALL FUNCTION 'K_PERFORMANCE_STOP'
      EXPORTING
        i_tabname        = con_table-makt
        i_record_counter = sy-tfill.
  ENDIF.

*..read the offsetting account texts
  IF NOT gt_gkont[] IS INITIAL AND
     NOT v_text_flags-account IS INITIAL.

    CALL FUNCTION 'K_PERFORMANCE_START'
      EXPORTING
        i_tabname = con_table-skat.

*...delete read cost elements from key table
    LOOP AT gt_gkont_master.
      DELETE gt_gkont WHERE  gkoar = gt_gkont_master-gkoar
                      AND    gkont = gt_gkont_master-gkont.
    ENDLOOP.

    PERFORM read_master_gkont.
    DESCRIBE TABLE gt_gkont LINES sy-tfill.

    CALL FUNCTION 'K_PERFORMANCE_STOP'
      EXPORTING
        i_tabname        = con_table-skat
        i_record_counter = sy-tfill.

  ENDIF.

*..read the purchase order texts
  IF NOT gt_ebeln[] IS INITIAL AND
     NOT v_text_flags-purchase_order IS INITIAL.

    CALL FUNCTION 'K_PERFORMANCE_START'
      EXPORTING
        i_tabname = con_table-ekpo.

*...delete read purchase orders from key table
    LOOP AT gt_ebeln_master.
      DELETE gt_ebeln WHERE  ebelp = gt_ebeln_master-ebelp
                      AND    ebeln = gt_ebeln_master-ebeln.
    ENDLOOP.

    PERFORM read_master_ebeln.
    DESCRIBE TABLE gt_ebeln LINES sy-tfill.

    CALL FUNCTION 'K_PERFORMANCE_STOP'
      EXPORTING
        i_tabname        = con_table-ekpo
        i_record_counter = sy-tfill.
  ENDIF.
ENDFORM.                               " READ_MASTER_DATA

*&---------------------------------------------------------------------*
FORM read_master_controlling_area
*----------------------------------------------------------------------*
USING VALUE(v_usear)      TYPE kaep_flag.
*----------------------------------------------------------------------*

  CALL FUNCTION 'K_KOKRS_READ'
    EXPORTING
      kokrs   = gd-kokrs
    IMPORTING
      e_tka01 = gs_tka01.

* if reported from archive, controlling area currency might be different
  IF NOT v_usear IS INITIAL.
    CALL FUNCTION 'K_KOKRS_CURRENCY_GET'
      EXPORTING
        i_kokrs   = gd-kokrs
      IMPORTING
        e_waers   = gs_tka01-waers
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      PERFORM message USING sy-msgid sy-msgty sy-msgno
                            sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.                               " READ_MASTER_CONTROLLING_AREA

*&---------------------------------------------------------------------*
FORM read_master_objects
*&---------------------------------------------------------------------*
USING VALUE(v_read_texts)  TYPE c
      VALUE(v_read_groups) TYPE c
      VALUE(v_item_group)  TYPE kaep_item_group
      VALUE(v_rep_object)  TYPE kaep_rep_object
      VALUE(v_usear)       TYPE kaep_flag
      VALUE(vt_selection)       TYPE kaep_t_cosel.
*&---------------------------------------------------------------------*
  INCLUDE rbonrart.

  DATA: ld_objnr        LIKE covp-objnr,
        ld_objnr_ext    LIKE ccr1z-objnr,
        ld_text         LIKE sy-msgli,
        ld_ident        LIKE sy-msgv1,
        ld_field        TYPE kaep_fieldname,
        ld_subrc        LIKE sy-subrc,
        ld_sav_obart    LIKE ionra-obart,
        ls_ionra        LIKE ionra,
        ls_sub          TYPE kaep_sub_groups,
        lt_groups       TYPE kaep_groups,
        ls_objnr_master TYPE kaep_objnr_master,
        ls_cosel        TYPE kaep_cosel.

  DATA: ls_aufkv LIKE aufkv.

  FIELD-SYMBOLS: <ld_fv>.              "field value

  LOOP AT gt_objnr INTO ld_objnr.
    CLEAR ls_objnr_master.
    CALL FUNCTION 'OBJECT_IDENTIFICATION_GET'
      EXPORTING
        datum       = sy-datlo
        langu       = sy-langu
        no_bukrs    = 'X'
        no_erkrs    = 'X'
        no_kokrs    = 'X'
        objnr       = ld_objnr
*       TEXT_WANTED = V_READ_TEXTS
      IMPORTING
        e_ionra     = ls_ionra
        e_text      = ld_text
        ident_objid = ld_ident
        ident_txt20 = ls_objnr_master-objart_txt
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc = 0.
      IF (     ld_text IS INITIAL AND
           NOT v_usear IS INITIAL ).
*       object text may be archived
        PERFORM get_object_text_from_archive USING    ld_objnr
                                             CHANGING ld_text.
      ENDIF.
* in case of ALE scenarios only 'PR' might be provided (see note 761732)
      ls_ionra-objnr = ld_objnr.                            "note761732
      ls_objnr_master-objid   = ld_ident.
      ls_objnr_master-obj_txt = ld_text.
      MOVE-CORRESPONDING ls_ionra TO ls_objnr_master.
      IF cl_erp_co_olc_sw_check=>erp_co_olc( ) = 'X' AND
         ls_ionra-obart = objektart_ov.
        CALL FUNCTION 'CO_SF_AFVG_CHECK_WITH_KEY'
          EXPORTING
            aplzl  = ls_ionra-aplzl
            aufpl  = ls_ionra-aufpl
          IMPORTING
            vornr  = ls_objnr_master-vornr_auf
          EXCEPTIONS
            OTHERS = 1.
        CALL FUNCTION 'CO_SF_AFVG_OBJECT_GET'
          EXPORTING
            aplzl          = ls_ionra-aplzl
            aufpl          = ls_ionra-aufpl
            exp_only_aufkv = 'X'
          IMPORTING
            aufkv_exp      = ls_aufkv
          EXCEPTIONS
            OTHERS         = 1.
        ls_objnr_master-aufnr = ls_aufkv-aufnr.
*       LS_OBJNR_MASTER-OBJ_TXT = ls_aufkv-ktext.         "1738502
      ENDIF.
* get further fields for projects
      IF v_rep_object = objektart_pd AND
        ( ls_objnr_master-objnr(2) <> objektart_ao AND
          ls_objnr_master-objnr(2) <> objektart_bp AND
          ls_objnr_master-objnr(2) <> objektart_eo AND
          ls_objnr_master-objnr(2) <> objektart_hp AND
          ls_objnr_master-objnr(2) <> objektart_ia AND
          ls_objnr_master-objnr(2) <> objektart_ib AND
          ls_objnr_master-objnr(2) <> objektart_im AND
          ls_objnr_master-objnr(2) <> objektart_is AND
          ls_objnr_master-objnr(2) <> objektart_iv AND
          ls_objnr_master-objnr(2) <> objektart_iw AND
          ls_objnr_master-objnr(2) <> objektart_kl AND
          ls_objnr_master-objnr(2) <> objektart_ks AND
          ls_objnr_master-objnr(2) <> objektart_op ). "note 1944025
        CALL FUNCTION 'PS05_OBJNR_MASTER_GET'
          EXPORTING                                        "2433701
            it_additional_objects = gt_add_objnr           "2433701
          CHANGING
            cs_master_objnr       = ls_objnr_master.
      ELSEIF ls_objnr_master-objnr(2) = objektart_kl OR
             ls_objnr_master-objnr(2) = objektart_ks. "note 1944025
* determine long text for Object
        PERFORM get_object_ltext USING ld_objnr
                              CHANGING ls_objnr_master-obj_ltxt.
      ENDIF.                                          "note 1944025
* read object currency
      CALL FUNCTION 'COUT_GET_OWAER'
        EXPORTING
          i_ionra = ls_ionra
        IMPORTING
          owaer   = ls_objnr_master-owaer
        EXCEPTIONS
          OTHERS  = 1.
* start of note 846067
      IF sy-subrc <> 0.
        CALL FUNCTION 'K_OBJECT_INFORMATION_GET'
          EXPORTING
            i_objnr = ls_ionra-objnr
          IMPORTING
            e_owaer = ls_objnr_master-owaer
          EXCEPTIONS
            OTHERS  = 1.
* end of note 846067
        IF sy-subrc <> 0.
          CLEAR ls_objnr_master-owaer.
          IF v_usear IS INITIAL.
            ld_objnr_ext = ls_ionra-objnr.
            WRITE ld_objnr_ext TO sy-msgv1.
            PERFORM message USING 'KB' 'I' '474' sy-msgv1
                                  space space space.
            IF 1 = 2.
              MESSAGE i474(kb) WITH sy-msgv1.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ls_objnr_master-owaer IS INITIAL
      AND NOT v_usear IS INITIAL.
        CALL FUNCTION 'KARL_MASTERDATA_GET'
          EXPORTING
            i_objnr   = ld_objnr
          IMPORTING
            e_owaer   = ls_objnr_master-owaer
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc <> 0.
          CALL FUNCTION 'K_OBJECT_INFORMATION_GET'
            EXPORTING
              i_objnr = ld_objnr
            IMPORTING
              e_owaer = ls_objnr_master-owaer
            EXCEPTIONS
              OTHERS  = 1.
          IF sy-subrc <> 0.
            ld_objnr_ext = ld_objnr.
            WRITE ld_objnr_ext TO sy-msgv1.
            PERFORM message USING 'KB' 'I' '474' sy-msgv1
                                  space space space.
            IF 1 = 2.
              MESSAGE i474(kb) WITH sy-msgv1.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
* read special object settlement information
      IF v_item_group = con_item_group-settlement OR
         v_item_group = con_item_group-retirement.

        READ TABLE vt_selection WITH KEY field = con_field-objnr
                                         low = ls_ionra-objnr
             INTO ls_cosel.
        IF sy-subrc = 0.
          PERFORM im_read_master_object_set_info USING ls_ionra
                                          CHANGING ls_objnr_master.
        ENDIF.
      ENDIF.
*.... if the group is in the display variant
      IF NOT v_read_groups IS INITIAL.
*......new object type ==> set the fields and read the set
        IF ld_sav_obart <> ls_ionra-obart.
          CASE ls_ionra-obart.
            WHEN objektart_ks.
              ld_field = con_field-kostl.
            WHEN objektart_kl.
              ld_field = con_field-kostl.
            WHEN objektart_or.
              ld_field = con_field-aufnr.
            WHEN objektart_bp.
              ld_field = con_field-prznr.
            WHEN objektart_hp.
              ld_field = con_field-kstrg.
            WHEN OTHERS.
              CLEAR ld_field.
          ENDCASE.
          READ TABLE gt_groups INTO lt_groups
               WITH KEY field = ld_field TRANSPORTING sub.
          ld_subrc = sy-subrc.
        ENDIF.
*......object type supported ==> check the value with set
        IF ld_subrc = 0.
          ASSIGN COMPONENT ld_field OF STRUCTURE ls_ionra TO <ld_fv>.
          LOOP AT lt_groups-sub INTO ls_sub.
*...........value in set ==> take over the set information
            IF <ld_fv> >= ls_sub-from AND <ld_fv> <= ls_sub-to.
              MOVE ls_sub TO ls_objnr_master-sub.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
        ld_sav_obart = ls_ionra-obart.
      ENDIF.
      APPEND ls_objnr_master TO gt_objnr_master.
    ELSE.
      ld_objnr_ext = ld_objnr.
      WRITE ld_objnr_ext TO sy-msgv1.
      PERFORM message USING 'KB' 'I' '473' sy-msgv1
                            space space space.
      IF 1 = 2.
        MESSAGE i473(kb) WITH sy-msgv1.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT gt_objnr_master BY objnr.

ENDFORM.                               " READ_MASTER_OBJECTS

*&---------------------------------------------------------------------*
FORM read_master_cost_elements
  USING VALUE(v_read_groups) TYPE c.
*&---------------------------------------------------------------------*
  DATA: BEGIN OF lt_kstar OCCURS 0,
          kstar LIKE csku-kstar,
        END OF lt_kstar.
  DATA: ls_sub        TYPE kaep_sub_groups,
        lt_groups     TYPE kaep_groups,
        ld_tabix      LIKE sy-tabix,
        ld_kstar_from LIKE csku-kstar,
        ld_kstar_to   LIKE csku-kstar.

  LOOP AT gt_kstar INTO lt_kstar-kstar.
    APPEND lt_kstar.
  ENDLOOP.
  CHECK NOT lt_kstar[] IS INITIAL.
  LOOP AT lt_kstar.
    SELECT SINGLE kstar ktext ltext FROM csku INTO (gt_kstar_master-kstar,
                                               gt_kstar_master-cel_ktxt,
                                               gt_kstar_master-cel_ltxt)
                             WHERE spras = sy-langu
                             AND   ktopl = gs_tka01-ktopl
                             AND   kstar = lt_kstar-kstar.
    IF sy-subrc EQ 0.
      APPEND gt_kstar_master.
    ELSE.
      CLEAR gt_kstar_master.
      gt_kstar_master-kstar = lt_kstar-kstar.
      APPEND gt_kstar_master.
    ENDIF.
  ENDLOOP.
  SORT gt_kstar_master BY kstar.
*..get the group name for the cost element
  IF NOT v_read_groups IS INITIAL.
    READ TABLE gt_groups INTO lt_groups
         WITH KEY field = con_field-kstar TRANSPORTING sub.
    CHECK sy-subrc = 0.
    LOOP AT gt_kstar_master.
      ld_tabix = sy-tabix.
      LOOP AT lt_groups-sub INTO ls_sub.
*...........value in set ==> take over the set information
        ld_kstar_from = ls_sub-from.
        ld_kstar_to   = ls_sub-to.
        IF gt_kstar_master-kstar BETWEEN ld_kstar_from AND ld_kstar_to.
          MOVE ls_sub TO gt_kstar_master-sub.
          MODIFY gt_kstar_master INDEX ld_tabix TRANSPORTING sub.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " READ_MASTER_COST_ELEMENTS

*&---------------------------------------------------------------------*
FORM read_master_material.
*&---------------------------------------------------------------------*
  DATA: BEGIN OF lt_matnr OCCURS 0,
          matnr LIKE makt-matnr,
        END OF lt_matnr.

  LOOP AT gt_matnr INTO lt_matnr-matnr.
    APPEND lt_matnr.
  ENDLOOP.
  CHECK NOT lt_matnr[] IS INITIAL.
  SELECT maktx matnr FROM makt INTO (gt_matnr_master-mat_txt,
                                     gt_matnr_master-matnr)
                                     FOR ALL ENTRIES IN lt_matnr
           WHERE matnr = lt_matnr-matnr AND
                 spras = sy-langu.
    APPEND gt_matnr_master.
  ENDSELECT.

  SORT gt_matnr_master BY matnr.

ENDFORM.                               " READ_MASTER_MATERIAL

*&---------------------------------------------------------------------*
FORM read_master_qmnum.
*&---------------------------------------------------------------------*
  DATA: BEGIN OF lt_qmnum OCCURS 0,
          qmnum LIKE qmel-qmnum,
        END OF lt_qmnum.

  LOOP AT gt_qmnum INTO lt_qmnum-qmnum .
    APPEND lt_qmnum.
  ENDLOOP.
  CHECK NOT lt_qmnum[] IS INITIAL.
  SELECT qmnum qmtxt FROM qmel INTO
              (gt_qmnum_master-qmnum, gt_qmnum_master-qmtxt)
                                     FOR ALL ENTRIES IN lt_qmnum
           WHERE qmnum = lt_qmnum-qmnum.
    APPEND gt_qmnum_master.
  ENDSELECT.

  SORT gt_qmnum_master BY qmnum.

ENDFORM.                               " READ_MASTER_MATERIAL

*&---------------------------------------------------------------------*
FORM read_master_gkont.
*&---------------------------------------------------------------------*
  DATA: lt_gkont_cred TYPE kaep_t_gkont_keys,
        lt_gkont_deb  TYPE kaep_t_gkont_keys,
        lt_gkont_acc  TYPE kaep_t_gkont_keys.
  DATA: ls_gkont_cred TYPE kaep_gkont_keys,
        ls_gkont_deb  TYPE kaep_gkont_keys,
        ls_gkont_acc  TYPE kaep_gkont_keys.

  LOOP AT gt_gkont.
    CASE gt_gkont-gkoar.
      WHEN 'K'.
        ls_gkont_cred-gkont = gt_gkont-gkont.
        APPEND ls_gkont_cred TO lt_gkont_cred.
      WHEN 'D'.
        ls_gkont_deb-gkont  = gt_gkont-gkont.
        APPEND ls_gkont_deb TO lt_gkont_deb.
      WHEN OTHERS.
        ls_gkont_acc-gkont  = gt_gkont-gkont.
        APPEND ls_gkont_acc TO lt_gkont_acc.
    ENDCASE.
  ENDLOOP.
  IF NOT lt_gkont_deb[] IS INITIAL.
    CALL FUNCTION 'K_DEBITORS_READ_TEXTS'
      EXPORTING
        it_gkont_keys      = lt_gkont_deb
      CHANGING
        ct_gkont_and_texts = gt_gkont_master[]
      EXCEPTIONS
        OTHERS             = 0.
  ENDIF.
  IF NOT lt_gkont_cred[] IS INITIAL.
    CALL FUNCTION 'K_CREDITORS_READ_TEXTS'
      EXPORTING
        it_gkont_keys      = lt_gkont_cred
      CHANGING
        ct_gkont_and_texts = gt_gkont_master[]
      EXCEPTIONS
        OTHERS             = 0.
  ENDIF.
  IF NOT lt_gkont_acc[] IS INITIAL.
    CALL FUNCTION 'K_ACCOUNTS_READ_TEXTS'
      EXPORTING
        i_ktopl            = gs_tka01-ktopl
        it_gkont_keys      = lt_gkont_acc
      CHANGING
        ct_gkont_and_texts = gt_gkont_master[]
      EXCEPTIONS
        OTHERS             = 0.

  ENDIF.

  SORT gt_gkont_master BY gkoar gkont.

ENDFORM.                               " READ_MASTER_MATERIAL
*&---------------------------------------------------------------------*
FORM read_master_ebeln.
*&---------------------------------------------------------------------*

  CHECK NOT gt_ebeln[] IS INITIAL.

  CALL FUNCTION 'K_PURCHASE_ORDERS_READ_TEXTS'
    EXPORTING
      it_ebeln_keys = gt_ebeln[]
    CHANGING
      ct_ebtxt      = gt_ebeln_master[]
    EXCEPTIONS
      OTHERS        = 0.


ENDFORM.                               " READ_MASTER_MATERIAL
*&---------------------------------------------------------------------*
FORM add_lines_count
*&---------------------------------------------------------------------*
TABLES   t_range
CHANGING c_lines.
*----------------------------------------------------------------------*
  DESCRIBE TABLE t_range LINES sy-tfill.
  c_lines = c_lines + sy-tfill.
ENDFORM.                               " ADD_LINES

*&---------------------------------------------------------------------*
FORM clc_blocs
*&---------------------------------------------------------------------*
USING    VALUE(v_entries)
         VALUE(v_bloc_size)
CHANGING VALUE(c_blocs).
*&---------------------------------------------------------------------*

  DATA: ld_mod LIKE sy-index.

  ld_mod =  v_entries MOD v_bloc_size.
  c_blocs = v_entries DIV v_bloc_size.
  IF ld_mod > 0.
    ADD 1 TO c_blocs.
  ENDIF.
ENDFORM.                    "clc_blocs

*&---------------------------------------------------------------------*
FORM fill_bloc
*&---------------------------------------------------------------------*
TABLES t_bloc
       t_range
*&---------------------------------------------------------------------*
USING VALUE(v_index)     LIKE sy-index
      VALUE(v_bloc_size) LIKE sy-tfill.
*&---------------------------------------------------------------------*
  DATA: ld_index LIKE sy-index.

  REFRESH t_bloc.
  ld_index = ( v_index - 1 ) * v_bloc_size + 1.

  DO v_bloc_size TIMES.
    READ TABLE t_range INDEX ld_index.
    IF sy-subrc = 0.
      t_bloc = t_range.
      APPEND t_bloc.
    ELSE.
      EXIT.
    ENDIF.
    ld_index = ld_index + 1.
  ENDDO.

ENDFORM.                    "fill_bloc

*&---------------------------------------------------------------------*
FORM collect_master_keys_covp
*&---------------------------------------------------------------------*
* Collect filled key fields from covp into master data tables
*----------------------------------------------------------------------*
 USING   VALUE(vs_covp) LIKE covp.
*----------------------------------------------------------------------*
  DATA: ls_ebeln TYPE kaep_ebeln.
  DATA: ls_add_objnr LIKE LINE OF gt_add_objnr.             "2433701

  IF NOT vs_covp-objnr IS INITIAL.
    COLLECT vs_covp-objnr    INTO gt_objnr.
  ENDIF.
* only select PAROB1 (PAROB may only contain 'OR')
  IF NOT vs_covp-parob1 IS INITIAL.
*   2433701
    IF sy-cprog = 'RKPEP003' AND
      ( vs_covp-parob1(2) <> objektart_ao AND
        vs_covp-parob1(2) <> objektart_bp AND
        vs_covp-parob1(2) <> objektart_eo AND
        vs_covp-parob1(2) <> objektart_hp AND
        vs_covp-parob1(2) <> objektart_ia AND
        vs_covp-parob1(2) <> objektart_ib AND
        vs_covp-parob1(2) <> objektart_im AND
        vs_covp-parob1(2) <> objektart_is AND
        vs_covp-parob1(2) <> objektart_iv AND
        vs_covp-parob1(2) <> objektart_iw AND
        vs_covp-parob1(2) <> objektart_kl AND
        vs_covp-parob1(2) <> objektart_ks AND
        vs_covp-parob1(2) <> objektart_op ).
      IF NOT vs_covp-parob1 IN gr_objnr.
        COLLECT vs_covp-parob1 INTO gt_add_objnr.
      ENDIF.
    ENDIF.
    COLLECT vs_covp-parob1    INTO gt_objnr.
  ENDIF.
  IF NOT vs_covp-uspob IS INITIAL.
    COLLECT vs_covp-uspob    INTO gt_objnr.
  ENDIF.
  IF NOT vs_covp-gkont IS INITIAL.
    gt_gkont-gkont = vs_covp-gkont.
    gt_gkont-gkoar = vs_covp-gkoar.
    COLLECT gt_gkont.
  ENDIF.
  IF NOT vs_covp-matnr IS INITIAL.
    COLLECT vs_covp-matnr  INTO gt_matnr.
  ENDIF.
  IF NOT vs_covp-kstar IS INITIAL.
    COLLECT vs_covp-kstar  INTO gt_kstar.
  ENDIF.
  IF NOT vs_covp-ebeln IS INITIAL.
    ls_ebeln-ebeln = vs_covp-ebeln.
    ls_ebeln-ebelp = vs_covp-ebelp.
    COLLECT ls_ebeln  INTO gt_ebeln.
  ENDIF.
  IF NOT vs_covp-qmnum IS INITIAL.
    COLLECT vs_covp-qmnum INTO gt_qmnum.
  ENDIF.

ENDFORM.                               " COLLECT_MASTER_KEYS_COVP
*&---------------------------------------------------------------------*
FORM collect_master_keys_covpb
*&---------------------------------------------------------------------*
* Collect filled key fields from covpb into master data tables
*----------------------------------------------------------------------*
 USING   VALUE(vs_covpb) LIKE covpb.
*----------------------------------------------------------------------*
  DATA: ls_ebeln TYPE kaep_ebeln.

  IF NOT vs_covpb-objnr IS INITIAL.
    COLLECT vs_covpb-objnr    INTO gt_objnr.
  ENDIF.
* only select PAROB1 (PAROB may only contain 'OR')
  IF NOT vs_covpb-parob1 IS INITIAL.
    COLLECT vs_covpb-parob1    INTO gt_objnr.
  ENDIF.
  IF NOT vs_covpb-kstar IS INITIAL.
    COLLECT vs_covpb-kstar  INTO gt_kstar.
  ENDIF.

ENDFORM.                               " COLLECT_MASTER_KEYS_COVPb
*&---------------------------------------------------------------------*
FORM collect_master_keys_covj
*&---------------------------------------------------------------------*
* Collect filled key fields from covj into master data tables
*----------------------------------------------------------------------*
 USING   VALUE(vs_covj) LIKE covj.
*----------------------------------------------------------------------*
  DATA: ls_ebeln TYPE kaep_ebeln.
  DATA: ls_add_objnr LIKE LINE OF gt_add_objnr.             "2433701

  IF NOT vs_covj-objnr IS INITIAL.
    COLLECT vs_covj-objnr    INTO gt_objnr.
  ENDIF.
* only select PAROB1 (PAROB may only contain 'OR')
  IF NOT vs_covj-parob1 IS INITIAL.
*   2433701
    IF sy-cprog = 'RKPEP004' AND
      ( vs_covj-parob1(2) <> objektart_ao AND
        vs_covj-parob1(2) <> objektart_bp AND
        vs_covj-parob1(2) <> objektart_eo AND
        vs_covj-parob1(2) <> objektart_hp AND
        vs_covj-parob1(2) <> objektart_ia AND
        vs_covj-parob1(2) <> objektart_ib AND
        vs_covj-parob1(2) <> objektart_im AND
        vs_covj-parob1(2) <> objektart_is AND
        vs_covj-parob1(2) <> objektart_iv AND
        vs_covj-parob1(2) <> objektart_iw AND
        vs_covj-parob1(2) <> objektart_kl AND
        vs_covj-parob1(2) <> objektart_ks AND
        vs_covj-parob1(2) <> objektart_op ).
      IF NOT vs_covj-parob1 IN gr_objnr.
        COLLECT vs_covj-parob1 INTO gt_add_objnr.
      ENDIF.
    ENDIF.
    COLLECT vs_covj-parob1    INTO gt_objnr.
  ENDIF.
  IF NOT vs_covj-uspob IS INITIAL.
    COLLECT vs_covj-uspob    INTO gt_objnr.
  ENDIF.
  IF NOT vs_covj-kstar IS INITIAL.
    COLLECT vs_covj-kstar  INTO gt_kstar.
  ENDIF.

ENDFORM.                               " COLLECT_MASTER_KEYS_COVP
*&---------------------------------------------------------------------*
FORM collect_master_keys_cooi
*&---------------------------------------------------------------------*
* Collect filled key fields from cooi into master data tables
*----------------------------------------------------------------------*
 USING   VALUE(vs_cooi) LIKE cooi.
*----------------------------------------------------------------------*

  IF NOT vs_cooi-objnr IS INITIAL.
    COLLECT vs_cooi-objnr    INTO gt_objnr.
  ENDIF.
  IF NOT vs_cooi-matnr IS INITIAL.
    COLLECT vs_cooi-matnr  INTO gt_matnr.
  ENDIF.
  IF NOT vs_cooi-sakto IS INITIAL.
    COLLECT vs_cooi-sakto  INTO gt_kstar.
  ENDIF.

ENDFORM.                               " COLLECT_MASTER_KEYS_COOI

*&---------------------------------------------------------------------*
FORM collect_master_keys_v_cofp
*&---------------------------------------------------------------------*
* Collect filled key fields from v_cofp into master data tables
*----------------------------------------------------------------------*
 USING   VALUE(vs_v_vofp) LIKE v_cofp.
*----------------------------------------------------------------------*

  IF NOT vs_v_vofp-objnr IS INITIAL.
    COLLECT vs_v_vofp-objnr INTO gt_objnr.
  ENDIF.

ENDFORM.                               " COLLECT_MASTER_KEYS_V_COFP

*&---------------------------------------------------------------------*
*&      Form  IM_READ_MASTER_OBJECT_SETTL_INFO
*&---------------------------------------------------------------------*
*       like company code, depr.areas, request cc,...
*----------------------------------------------------------------------*
*      -->P_LS_IONRA  text                                             *
*      <--P_LS_OBJECT_MASTER  text                                     *
*----------------------------------------------------------------------*
FORM im_read_master_object_set_info USING    p_ionra LIKE ionra
                                   CHANGING p_objnr_master
                                             TYPE kaep_objnr_master.
  DATA: lt_anli          LIKE anli    OCCURS 1 WITH HEADER LINE,
        lt_coiob         LIKE coiob   OCCURS 1 WITH HEADER LINE,
        lt_periods       LIKE periods OCCURS 1 WITH HEADER LINE,
        ld_basic_valutyp LIKE tka09-valutyp,
        lt_tka09v        LIKE v_tka09_v OCCURS 1,
        ld_lines         TYPE p,
        ls_taprf         LIKE taprf.
  REFRESH lt_anli.
  REFRESH lt_coiob.
  p_objnr_master-cobr_info-kokrs = gd-kokrs.


  CALL FUNCTION 'K_VERSN_VALUATION'
    EXPORTING
      i_kokrs           = gd-kokrs
    IMPORTING
      e_basic_valuation = ld_basic_valutyp
    TABLES
      t_tka09_v         = lt_tka09v.
  DESCRIBE TABLE lt_tka09v LINES ld_lines.
*  if ld_lines > 1.   " es werden mehrere Versionen verwendet
*    if ld_basic_valutyp <> con_valutyp_0. " Basisversion <> legal
*      message e651(aw).
*    endif.
*    message s652(aw).                  " Anzeige nur der Basisversion
*  endif.
  CALL FUNCTION 'ANLI_READ_MULTIPLE_OBJNR'
    EXPORTING
      i_objnr = p_ionra-objnr
    TABLES
      t_anli  = lt_anli.
  READ TABLE lt_anli INDEX 1.
  IF sy-subrc <> 0.
    MESSAGE e253(aw).
  ELSE.

    CALL FUNCTION 'IM_TAPRF_READ'
      EXPORTING
        i_ivpro = lt_anli-ivpro
      IMPORTING
        e_taprf = ls_taprf.
    IF ls_taprf-abart <> con_on.
      MESSAGE e255(aw) WITH lt_anli-ivpro.
    ENDIF.
  ENDIF.
  p_objnr_master-ivpro = lt_anli-ivpro.
  lt_coiob-objnr = p_ionra-objnr.
  APPEND lt_coiob.

  CALL FUNCTION 'K_OBJECTS_MASTER_READ'
    TABLES
      coiob_tab    = lt_coiob
      i_periods    = lt_periods
    EXCEPTIONS
      error_occurs = 1
      wrong_object = 2
      OTHERS       = 3.
  READ TABLE lt_coiob INDEX 1.
  p_objnr_master-cobr_info-bukrs = lt_coiob-bukrs.
  p_objnr_master-cobr_info-abukr = lt_anli-bukrs.
  IF p_ionra-obart = objektart_pr.                          "ins 442614
    p_objnr_master-posid = lt_coiob-posid.                  "ins 442614
  ENDIF.                                                    "ins 442614
  IF gd-update = con_on.
    CALL FUNCTION 'ENQUEUE_EKCOBR'
      EXPORTING
        objnr = p_objnr_master-objnr.
  ENDIF.
  CHECK gt_t093_info[] IS INITIAL.
  PERFORM im_read_t093_info TABLES lt_anli
                              gt_t093_info.

ENDFORM.                               " READ_MASTER_OBJECT_SETTL_INFO
*&---------------------------------------------------------------------*
FORM read_archive
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
TABLES t_arsel                  STRUCTURE rng_archiv
USING  VALUE(v_kokrs)           TYPE      kokrs
       VALUE(v_arobj)           LIKE      arch_obj-object
       VALUE(v_item_group)      TYPE      kaep_item_group
       VALUE(v_rep_object)      TYPE      kaep_rep_object
       VALUE(vt_selection)      TYPE      kaep_t_cosel
       VALUE(vt_free_selection) TYPE      rsds_trange.
*----------------------------------------------------------------------*
  DATA: lt_arch_obj  LIKE arch_obj-object OCCURS 0 WITH HEADER LINE,
        ld_callback  LIKE arch_obj-exit_rout,
        ld_report    TYPE  progname,
        lt_selection TYPE rsds_trange WITH HEADER LINE,
        ls_cosel     TYPE kaep_cosel,
        ls_frange    TYPE rsds_frange,
        lt_sel       TYPE rsds_frange_t WITH HEADER LINE,
        ls_selopt    LIKE rsdsselopt,
        lt_tables    TYPE kaep_tabname OCCURS 0.

  IF sy-batch IS INITIAL AND gd-called_by_rri IS INITIAL.
    DESCRIBE TABLE <gt_pos_data> LINES sy-tfill.
    IF sy-tfill GE gd-maxsel.
      EXIT.
    ENDIF.
    DESCRIBE TABLE <gt_head_data> LINES sy-tfill.
    IF sy-tfill GE gd-maxsel.
      EXIT.
    ENDIF.
  ENDIF.
  APPEND v_arobj TO lt_arch_obj.
  CASE v_item_group.
    WHEN con_item_group-cost_actual.
      IF v_rep_object = con_rep_object-documents.
*        APPEND 'COBK' TO lt_tables.
*        APPEND 'COEP' TO lt_tables.
        lt_selection-tablename = space.
        ld_callback = 'FILL_ARCHIVE_COBK_COEP'.
        ld_report = 'KSB5'.
      ELSE.
*        APPEND 'COVP' TO lt_tables.
        lt_selection-tablename = 'COVP'.
        ld_callback = 'FILL_ARCHIVE_COVP'.
        CASE v_rep_object.
          WHEN objektart_ks.
            ld_report = 'KSB1'.
          WHEN objektart_or.
            ld_report = 'KOB1'.
          WHEN objektart_hp.
            ld_report = 'KKCS'.
          WHEN objektart_bp.
            ld_report = 'CPB1'.
          WHEN objektart_pd.
            ld_report = 'CJI3'.
          WHEN objektart_vb.
            ld_report = 'KVBI'.
        ENDCASE.
      ENDIF.
    WHEN con_item_group-cost_plan.
      IF v_rep_object = con_rep_object-documents.
*        APPEND 'COVJ' to lt_tables.
        lt_selection-tablename = space.
        ld_callback = 'FILL_ARCHIVE_COBK_COEJ'.
        ld_report = 'KABP'.
      ELSE.
        lt_selection-tablename = 'COVJ'.
        ld_callback = 'FILL_ARCHIVE_COVJ'.
        CASE v_rep_object.
          WHEN objektart_ks.
            ld_report = 'KSBP'.
          WHEN objektart_or.
            ld_report = 'KOBP'.
          WHEN objektart_bp.
            ld_report = 'CPBP'.
          WHEN objektart_pd.
            ld_report = 'CJI4'.
        ENDCASE.
      ENDIF.
    WHEN con_item_group-finance_actual.
      IF v_rep_object = con_rep_object-documents.
*        APPEND 'COBK' TO lt_tables.
*        APPEND 'COFP' TO lt_tables.
*        lt_selection-tablename = space.
      ELSE.
*        APPEND 'V_COFP' TO lt_tables.
*        lt_selection-tablename = 'V_COFP'.
      ENDIF.
  ENDCASE.

* report has to be filled -> else shortdump
*  check not ld_report is initial.

* overtake free selection
  lt_selection[] = vt_free_selection.
* append fix selections from cosel to rsds_trange
  LOOP AT vt_selection INTO ls_cosel.
    AT NEW field.
      IF NOT ls_frange IS INITIAL.
        APPEND ls_frange TO lt_selection-frange_t.
      ENDIF.
      CLEAR ls_frange.
      ls_frange-fieldname = ls_cosel-field.
    ENDAT.
    MOVE-CORRESPONDING ls_cosel TO ls_selopt.
    APPEND ls_selopt TO ls_frange-selopt_t.
  ENDLOOP.
  APPEND ls_frange TO lt_selection-frange_t.
  APPEND lt_selection.

** read archive (old version)
*  CALL FUNCTION 'RW_ARCHIVE_LINK'
*    EXPORTING
*      i_selections       = lt_selection[]
*      i_callback_program = self
*      i_callback_routine = 'FILL_ARCHIVE_DATA'
*    TABLES
*      t_arch_obj         = lt_arch_obj
*      t_files            = t_arsel
*      t_tables           = lt_tables.

  LOOP AT lt_selection.
    LOOP AT lt_selection-frange_t INTO lt_sel.
      APPEND lt_sel.
    ENDLOOP.
  ENDLOOP.

* provide controlling area
  IF NOT v_kokrs IS INITIAL.
    ls_frange-fieldname = con_field-kokrs.
    ls_selopt-sign      = 'I'.
    ls_selopt-option    = 'EQ'.
    ls_selopt-low       = v_kokrs.
    CLEAR: ls_selopt-high, ls_frange-selopt_t[].
    APPEND ls_selopt TO ls_frange-selopt_t.
    APPEND ls_frange TO lt_sel.
  ENDIF.

* read archive (new version)
  CALL FUNCTION 'CO_EP_SELECT_FROM_ARCHIVE'
    EXPORTING
      i_selections       = lt_sel[]
      i_callback_program = self
      i_callback_routine = ld_callback
      i_report           = ld_report
      i_object           = v_arobj
      i_infosys          = gd-infosys
    TABLES
      t_files            = t_arsel.

ENDFORM.                               " READ_ARCHIVE
*&---------------------------------------------------------------------*
FORM gui_counter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
TABLES t_data
*----------------------------------------------------------------------*
CHANGING c_exit TYPE kaep_flag.
*----------------------------------------------------------------------*
  DATA: i            TYPE i,
        char(100)    TYPE c,
        char1(6)     TYPE c,
        ld_con_hex02 TYPE x VALUE '02'.
  STATICS sd_count TYPE i.

  DESCRIBE TABLE t_data LINES sy-tfill.
* don't check max lines in batch and if called by RRI
  IF sy-batch IS INITIAL AND gd-called_by_rri IS INITIAL.
    IF sy-tfill >= gd-maxsel.
      c_exit = 'X'.
      MESSAGE s203(kb) WITH sy-tfill gd-maxsel.
      EXIT.
    ENDIF.
  ENDIF.
* only show counter if it is not batch or start with printing
  IF NOT sy-subty O ld_con_hex02.
    CASE sy-tfill.
      WHEN 1.
        sd_count = con_guict.
      WHEN 1000.
        sd_count = con_guict_1000.
      WHEN 10000.
        sd_count = con_guict_10000.
    ENDCASE.
    i = sy-tfill MOD sd_count.
    IF i = 0.
      WRITE TEXT-gu1    TO char.
      WRITE sy-tfill TO char1.
      REPLACE '&' WITH char1 INTO char.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = char.
    ENDIF.
  ENDIF.
ENDFORM.                               " GUI_COUNTER

*---------------------------------------------------------------------*
FORM select_cobk_cofp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
USING VALUE(v_callback_programm) LIKE sy-repid
      VALUE(vt_var_cond_pos)     TYPE kaep_t_var_cond
      VALUE(vt_var_cond_head)    TYPE kaep_t_var_cond
      VALUE(vt_fields_pos)       TYPE kaep_t_fieldname
      VALUE(vt_fields_head)      TYPE kaep_t_fieldname
      VALUE(v_pos_table)         TYPE kaep_tabname
      VALUE(v_head_table)        TYPE kaep_tabname.
*----------------------------------------------------------------------*

  DATA: ls_v_cofp LIKE v_cofp,
        ls_t001   LIKE t001,
        ld_exit   TYPE kaep_flag.

* read document (head)
  SELECT (vt_fields_head)
         APPENDING CORRESPONDING FIELDS OF TABLE gt_cobk_ext
         FROM (v_head_table)
         WHERE kokrs = gd-kokrs
           AND   belnr IN gr_belnr
           AND   (vt_var_cond_head).

  CHECK NOT gt_cobk_ext[] IS INITIAL.

  SELECT (vt_fields_pos) INTO CORRESPONDING FIELDS OF gs_cofp_ext
         FROM (v_pos_table) FOR ALL ENTRIES IN gt_cobk_ext
         WHERE kokrs = gt_cobk_ext-kokrs
           AND belnr = gt_cobk_ext-belnr
*          AND versn in gr_versn
           AND (vt_var_cond_pos)
         ORDER BY PRIMARY KEY.
    APPEND gs_cofp_ext TO gt_cofp_ext.

*   collect master keys into global tables
    MOVE-CORRESPONDING gs_cofp_ext TO ls_v_cofp.
    PERFORM collect_master_keys_v_cofp USING ls_v_cofp.
    PERFORM gui_counter TABLES gt_cofp_ext
                        CHANGING ld_exit.
    IF NOT ld_exit IS INITIAL.
      EXIT.
    ENDIF.
  ENDSELECT.

* add information about fipos
  LOOP AT gt_cofp_ext INTO gs_cofp_ext.
    IF ls_t001-bukrs NE gs_cofp_ext-bukrs.
      SELECT SINGLE * FROM t001 INTO ls_t001
                      WHERE bukrs = gs_cofp_ext-bukrs.
    ENDIF.
    CALL FUNCTION 'GET_FIPOS_FROM_POSIT'
      EXPORTING
        ip_fikrs     = ls_t001-fikrs
        ip_fma_objnr = gs_cofp_ext-objnr
        ip_posit     = gs_cofp_ext-posit
      IMPORTING
*       OP_FIKRS     =
        op_fipos     = gs_cofp_ext-fipos
      EXCEPTIONS
        not_found    = 1
        OTHERS       = 2.
    IF sy-subrc = 0.
      MODIFY gt_cofp_ext FROM gs_cofp_ext.
    ENDIF.

  ENDLOOP.

ENDFORM.                               " SELECT_COBK_COFP
*&---------------------------------------------------------------------*
FORM gui_counter_archive
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
TABLES t_data
*----------------------------------------------------------------------*
CHANGING c_exit TYPE kaep_flag.
*----------------------------------------------------------------------*
  DATA: i            TYPE i,
        char(100)    TYPE c,
        char1(6)     TYPE c,
        ld_con_hex02 TYPE x VALUE '02'.

  DESCRIBE TABLE t_data LINES sy-tfill.
  i = sy-tfill MOD con_guict.
  IF i = 0.
    WRITE TEXT-gu1 TO char.
    WRITE sy-tfill TO char1.
    REPLACE '&' WITH char1 INTO char.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = char.
  ENDIF.

ENDFORM.                               " GUI_COUNTER_ARCHIVE

*&---------------------------------------------------------------------*
*&  FORM GET_OBJECT_TEXT_FROM_ARCHIVE
*&---------------------------------------------------------------------*
FORM get_object_text_from_archive USING    id_objnr TYPE j_objnr
                                  CHANGING cd_text  TYPE text60.

  DATA: ld_obart TYPE j_obart_ld,
        ls_csks  TYPE csks,
        ls_aufk  TYPE aufk,
        ls_prps  TYPE prps.

  ld_obart = id_objnr(2).

  CASE ld_obart.
    WHEN objektart_ks OR objektart_kl.
    WHEN objektart_or.
*       object text may be archived
      CALL FUNCTION 'KARL_MASTERDATA_GET'
        EXPORTING
          i_objnr      = id_objnr
        IMPORTING
          e_masterdata = ls_aufk
        EXCEPTIONS
          not_found    = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        CLEAR cd_text.
      ELSE.
        cd_text = ls_aufk-ktext.
      ENDIF.
    WHEN objektart_pr.
  ENDCASE.

ENDFORM.                               "GET_OBJECT_TEXT_FROM_ARCHIVE
*&---------------------------------------------------------------------*
*&      Form  get_object_ltext
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LD_OBJNR  text
*      <--P_LS_OBJNR_MASTER_OBJ_LTXT  text
*----------------------------------------------------------------------*
FORM get_object_ltext USING    id_objnr TYPE j_objnr
                      CHANGING cd_ltext TYPE text60.

  DATA: ld_obart TYPE j_obart_ld.
  DATA: ld_datum LIKE sy-datlo.

  DATA: ld_subrc LIKE sy-subrc.                             "1430703

  ld_obart = id_objnr(2).

  CASE ld_obart.
    WHEN objektart_ks OR objektart_kl.
      DATA: ls_cskt TYPE cskt.
      CALL FUNCTION 'READ_COSTCENTER_TEXT'
        EXPORTING
          datum          = sy-datum
          kokrs          = id_objnr+2(4)
          kostl          = id_objnr+6(10)
          sprache        = sy-langu
        IMPORTING
          text_wa        = ls_cskt
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
*        <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< begin of 1430703
        ld_datum = sy-datum.
        DO 5 TIMES.
          ld_datum = ld_datum - 365.
          CALL FUNCTION 'READ_COSTCENTER_TEXT'
            EXPORTING
              datum          = ld_datum
              kokrs          = id_objnr+2(4)
              kostl          = id_objnr+6(10)
              sprache        = sy-langu
            IMPORTING
              text_wa        = ls_cskt
            EXCEPTIONS
              text_not_found = 1
              OTHERS         = 2.
          ld_subrc = sy-subrc.
          IF ld_subrc = 0.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
      IF ld_subrc <> 0.
*     <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< end of 1430703
        CALL FUNCTION 'MESSAGES_ACTIVE'
          EXCEPTIONS
            not_active = 1.
        IF sy-subrc = 0.
          CALL FUNCTION 'MESSAGE_STORE'
            EXPORTING
              arbgb  = 'KS'
              msgty  = 'I'
              txtnr  = '013'
              msgv1  = id_objnr+6(10)
              msgv2  = sy-langu
            EXCEPTIONS
              OTHERS = 1.
        ENDIF.
      ELSE.
        cd_ltext = ls_cskt-ltext.
      ENDIF.

  ENDCASE.


ENDFORM.                               " get_object_ltext