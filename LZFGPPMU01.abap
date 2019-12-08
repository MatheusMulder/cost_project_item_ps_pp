FUNCTION z_bapi_co_cost_for_ppm.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(PROJECT_DEFI) TYPE  PS_PSPID
*"     VALUE(PERIOD_FROM) TYPE  CO_PERAB OPTIONAL
*"     VALUE(YEAR_FROM) TYPE  GJAHR OPTIONAL
*"     VALUE(PERIOD_TO) TYPE  CO_PERBI OPTIONAL
*"     VALUE(YEAR_TO) TYPE  GJAHR OPTIONAL
*"  EXPORTING
*"     VALUE(COST) TYPE  ZTTCO_COST_FOR_PPM
*"     VALUE(RETURN) TYPE  BAPIRET2_T
*"  EXCEPTIONS
*"      INVALID_ENTRY
*"----------------------------------------------------------------------

* Variables to set parameters from Z_BAPI_CO_COST_FOR_PPM
  DATA:
    lv_date_to     LIKE sy-datum,
    lv_date_from   LIKE sy-datum,
    ld_kokrs       LIKE coep-kokrs,
    lv_j_objid_out TYPE j_objid_out.

* Tables, structures and workareas
  DATA:
    lt_rsparams      TYPE TABLE OF  rsparams,
    lt_rsthie_m      TYPE TABLE OF rsthie_m,
    lt_psj_auth      TYPE TABLE OF psj_auth_m,
    lt_prps_rep      TYPE TABLE OF prps_cjdb_rep,
    lt_prps          TYPE TABLE OF prps,
    lt_proj_rep      TYPE TABLE OF proj_cjdb_rep,
    lt_proj          TYPE TABLE OF proj,
    lt_cosel         TYPE TABLE OF cosel,
    lt_cost_elm_des  TYPE TABLE OF ty_cost_element_descr,
    lt_wrttp         TYPE ps05_range_t_wrttp,
    lt_groups        TYPE kaep_t_groups,
    lt_select_fields TYPE rsfs_fields,
    lt_pd_selection  LIKE cosel OCCURS 0,
    ls_trange        TYPE rsds_trange,
    ls_tcndb         LIKE tcndb,
    ls_tcnds         LIKE tcnds,
    lw_proj          LIKE LINE OF lt_proj,
    lw_prps          LIKE LINE OF lt_prps,
    lw_cost          LIKE LINE OF cost.


*  Set parameters
  gv_project = project_defi.

  IF gv_project IS NOT INITIAL.

* Check if project exist in table PROJ
    PERFORM f_check_project_definition CHANGING return.

* Set date if they are empty
    IF period_from IS INITIAL OR year_from IS INITIAL.
      PERFORM f_set_empty_date_from  CHANGING lv_date_from.
      lv_date_from+6(2) = c_first_day.
    ELSE.
      lv_date_from =  year_from && period_from+1(2) && c_first_day.
    ENDIF.

    IF period_to IS INITIAL OR year_to IS INITIAL.
      PERFORM f_set_empty_date_to  CHANGING lv_date_to.
    ELSE.
      lv_date_to   =  year_to   && period_to+1(2)   && c_first_day.
    ENDIF.

* Get last day from moth
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_date_to
      IMPORTING
        last_day_of_month = lv_date_to
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.


* Check date interval
    PERFORM f_check_date_interval USING lv_date_from lv_date_to CHANGING return.

    READ TABLE return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc IS INITIAL.
      EXIT.
    ENDIF.


    CALL FUNCTION 'RS_REFRESH_FROM_DYNAMICAL_SEL'
      EXPORTING
        curr_report        = gv_repid
        mode_write_or_move = 'M'
      IMPORTING
        p_trange           = ls_trange
      EXCEPTIONS
        not_found          = 1.

* get selection table (rsparams)
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = gv_repid
      TABLES
        selection_table = lt_rsparams.

* Set parameters for select
    PERFORM f01_set_rsparams USING
                                  lv_date_from
                                  lv_date_to   CHANGING lt_rsparams lt_wrttp.

* fill table for field selection
    PERFORM f01_fill_select_fields TABLES lt_select_fields.

* import database profile for CJDB_GET_PROJECT_FROM_LDB_PSJ
    CALL FUNCTION 'PS06_TCNDB_TCNDS_IMPORT'
      IMPORTING
        e_tcndb = ls_tcndb
        e_tcnds = ls_tcnds.

    IF ls_tcndb IS INITIAL.
      PERFORM set_tcndb CHANGING ls_tcndb.
    ENDIF.

    gv_repid_f-handle = 'COAC'.
    gv_repid_f-log_group = 'PD'.


    CALL FUNCTION 'CJDB_GET_PROJECT_FROM_LDB_PSJ'
      EXPORTING
        i_psj_caller          = gv_repid_f-report
        i_trange              = ls_trange
        i_tcndb               = ls_tcndb
        i_tcnds               = ls_tcnds
        i_selection_set       = sy-slset
        i_sel_set_report      = gv_repid
        i_handle              = gv_handle
      TABLES
        t_rsparams            = lt_rsparams
        t_select_fields       = lt_select_fields
        t_rsthie_m            = lt_rsthie_m
        t_psj_auth_m          = lt_psj_auth
        t_proj                = lt_proj_rep
        t_prps_r              = lt_prps_rep
      EXCEPTIONS
        no_psj_caller         = 1
        no_selection_possible = 2
        no_data_found         = 3
        no_variant            = 4
        tcndb_missing         = 5
        tcnds_missing         = 6
        rsthie_m_empty        = 7.
    IF NOT sy-subrc      IS INITIAL OR
           lt_rsthie_m[] IS INITIAL.

      CALL FUNCTION 'HRIQ_APPEND_MESSAGE_TABLE'
        EXPORTING
          iv_msgtype   = 'I'
          iv_msgclass  = 'ZCOPPMCOST'
          iv_msgnumber = 001
        CHANGING
          ct_return    = return.

      EXIT.
    ENDIF.


*   Refill project master data into standard structures (for export)
*   -> better performance and display correct object id when reading
*      archives
    CALL FUNCTION 'CJDB_FREE_MEMORY_LDB_PSJ'.

    LOOP AT lt_proj_rep INTO DATA(lw_proj_rep).
      MOVE-CORRESPONDING lw_proj_rep TO lw_proj.
      APPEND lw_proj TO lt_proj.
    ENDLOOP.

    CALL FUNCTION 'CJPN_FILL_PROJ_BUFFER'
      TABLES
        t_proj_buf = lt_proj.

    FREE: lt_proj_rep,
          lt_proj.

    LOOP AT lt_prps_rep INTO DATA(lw_proj_repb).
      MOVE-CORRESPONDING lw_proj_repb TO lw_prps.
      APPEND lw_prps TO lt_prps.
    ENDLOOP.

    CALL FUNCTION 'CJPN_FILL_PRPS_BUFFER'
      TABLES
        t_prps_buf           = lt_prps
      EXCEPTIONS
        buffer_contains_data = 1.

    FREE: lt_prps_rep,
          lt_prps.

* fill selection fields
    PERFORM f01_fill_fields_for_selection
         TABLES   lt_rsparams
                  lt_wrttp
                  lt_rsthie_m
                  lt_cosel
                  lt_pd_selection
         USING    gv_called
                  gv_actual_v
         CHANGING gv_repid_f
                  ld_kokrs
                  lt_groups[]
                  gv_fixvar.


    CALL FUNCTION 'ZK_LINE_ITEMS_SELECT_AND_LIST'
      EXPORTING
        i_calling_report   = gv_repid_f-report
        i_item_group       = gv_repid_f-handle
        i_rep_object       = gv_repid_f-log_group
        i_display_variant  = '1SAP'
        i_co_area          = ld_kokrs
        i_actual_version   = gv_actual_v
        it_selection       = lt_cosel
        i_shownozero       = 'X'
        it_pd_selection    = lt_pd_selection
      IMPORTING
        t_kaep_t_covp_ext  = gt_ppm_cost
      EXCEPTIONS
        unknown_item_group = 1
        no_data_selected   = 2
        OTHERS             = 3.

    IF gt_ppm_cost IS INITIAL.

      CALL FUNCTION 'HRIQ_APPEND_MESSAGE_TABLE'
        EXPORTING
          iv_msgtype   = 'I'
          iv_msgclass  = 'ZCOPPMCOST'
          iv_msgnumber = 001
        CHANGING
          ct_return    = return.

      EXIT.
    ENDIF.

*   For performance purpose we gonna do only one call to the database.
    PERFORM get_cost_element_description CHANGING lt_cost_elm_des.

    LOOP AT gt_ppm_cost INTO DATA(lw_ppm_cost).
      IF lw_ppm_cost-orgvg NE c_koae.
        lw_cost-gjahr        = lw_ppm_cost-gjahr.
        lw_cost-perio        = lw_ppm_cost-perio.
        lw_cost-co_area      = lw_ppm_cost-bukrs.

        CALL FUNCTION 'OBJECT_IDENTIFICATION_GET'
          EXPORTING
            datum    = sy-datlo
            langu    = sy-langu
            no_bukrs = 'X'
            no_erkrs = 'X'
            no_kokrs = 'X'
            objnr    = lw_ppm_cost-objnr
          IMPORTING
            e_objid  = lv_j_objid_out
          EXCEPTIONS
            OTHERS   = 1.

        IF sy-subrc IS INITIAL.
          lw_cost-wbs_element  = lv_j_objid_out.
        ENDIF.

        lw_cost-cost_elem    = lw_ppm_cost-kstar.

        IF lt_cost_elm_des IS NOT INITIAL.
          SORT lt_cost_elm_des BY saknr.
          READ TABLE lt_cost_elm_des WITH KEY saknr = lw_ppm_cost-kstar INTO DATA(lw_cost_elm_des) BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            lw_cost-ce_descript = lw_cost_elm_des-txt50.
          ENDIF.
        ENDIF.

        lw_cost-ca_curr    =  lw_ppm_cost-kwaer.
        lw_cost-dc_ind     =  lw_ppm_cost-beknz.
        lw_cost-wkgbtr     =  lw_ppm_cost-wkgbtr.

        APPEND lw_cost TO cost.

      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFUNCTION.