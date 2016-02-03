;-----------------------------------------------------------------------
; module:	main_window
; date:		Jul 10th, 2007
;
; author:	Heinke Frerichs
; email:	hfrerichs@wisc.edu
;
; description:	main window of user interface
;
; contains:	make_data_base
;		make_plot_options_base
;		make_quit_base
;-----------------------------------------------------------------------



;=======================================================================
function make_opt_base, top_base

	base		= widget_base(top_base, frame=1)
	label		= widget_label(base, value='general plot options:')

	return, base
end; make_opt_base
;=======================================================================



;-----------------------------------------------------------------------
function make_plot_options_base, top_base
	common	event_id_cb

	;base		= widget_base (top_base, column=1, title='plot options', frame=1)
	base		= widget_base (top_base, column=1, title='plot options')
	base_label	= widget_label(base, value='volume data specific plot settings:', /align_left, resource_name='frame_title')

	; plot type
	tab		= widget_tab  (base, uname='plot_type_tab')
	type_base	= make_array(3, /int)
	type_strs	= ['2D poloidal cut', '1D profile', 'fieldline profile']
	for i=0,2 do begin
		type_base(i)	= widget_base (tab, title=type_strs(i), column=1, resource_name='plot_type')
	endfor


	; 2D poloidal cut ----------------------------------------------

	; toroidal angle
	frame		= widget_base (type_base(0), column=1, frame=1)
	tmp_base	= widget_base (frame, column=2, /grid_layout)
	tor_label	= widget_label(tmp_base, value='toroidal angle: ', /align_left)
	tor_text	= widget_text (tmp_base, value='0.1', /editable, uname=phi_select_id)

	; interface for selecting plot coordinates
	tmp_base	= widget_base (frame, column=2, /grid_layout)
	coord_label	= widget_label(tmp_base, value='plot coordinates: ')
	type_strs	= ['R - Z', 'theta - rmin', 'theta - psi*']
	type_cb		= widget_combobox (tmp_base, value=type_strs, uname=cb_plt_type_id)
	widget_control, type_cb, set_combobox_select=0
	set_plot_option, 0, /coords

	; data range
	tmp_base	= widget_base (frame, row=1)
	datar_label	= widget_label(tmp_base, value='data range: ', /align_center)
	data_min_text	= widget_text (tmp_base, /editable, uname='dmin', xsize=14)
	data_max_text	= widget_text (tmp_base, /editable, uname='dmax', xsize=14)
	b_clear		= widget_button(tmp_base, value='  clear  ', uname='b_clear_drange', /align_center)

	; select zoom
	tmp_frame	= widget_base (type_base(0), column=1, frame=1, xpad=0, ypad=0, space=0)

	; dropdown menu for selection pre-defined zooms
	tmp_base	= widget_base (tmp_frame, column=2, /grid_layout, frame=0, xpad=0, ypad=0, space=0)
	zoomlabel	= widget_label(tmp_base, value='Select pre-defined: ')
	zoom_cb		= widget_combobox (tmp_base, value='manual', uname='cb_zoom_predef', sensitive=0)
	update_zooms


	tmp_base	= widget_base (tmp_frame, column=4, /grid_layout, frame=0, xpad=0, ypad=0, space=0)
	;tmp_base	= widget_base (type_base(0), column=4, /grid_layout, frame=1, xpad=0, ypad=0, space=0)
	xlabel		= widget_label(tmp_base, value='[xmin, xmax]')
	ylabel		= widget_label(tmp_base, value='[ymin, ymax]')
	xmin_text	= widget_text (tmp_base, /editable, uname='xmin', xsize=10, sensitive=0)
	ymin_text	= widget_text (tmp_base, /editable, uname='ymin', xsize=10, sensitive=0)
	xmax_text	= widget_text (tmp_base, /editable, uname='xmax', xsize=10, sensitive=0)
	ymax_text	= widget_text (tmp_base, /editable, uname='ymax', xsize=10, sensitive=0)
	;b_select	= widget_button(tmp_base, value='select', uname='b_select_zoom')
	tmp2_base	= widget_base (tmp_base, column=1, /nonexclusive)
	tb		= widget_button(tmp2_base, value='set range', uname='set_range_id')

	; overlay data
	;b_overlay	= widget_button(type_base(0), value='overlay data', uname='b_overlay')

	; grid
	tmp_base	= widget_base  (type_base(0), column=1, /nonexclusive)
	toggle_button	= widget_button(tmp_base, value='plot grid', uname=plot_grid_id)
	set_plot_option, 0, /plot_grid

	; output for animated data
	;tmp_base	= widget_base  (type_base(0), column=1, /nonexclusive)
	;toggle_button	= widget_button(tmp_base, value='prepare animated output', uname=animated_id)
	;set_plot_option, 0, /animated

	; color table
	;tmp_base	= widget_base  (type_base(0), column=3)
	;b_select_ct	= widget_button(tmp_base, value='Select Colortable', uname='b_select_ct')
	;b_modify_ct	= widget_button(tmp_base, value='Modify Colortable', uname='b_modify_ct')
	;b_toggle_ct	= widget_base  (tmp_base, column=1, /nonexclusive)
	;b_auto_ct	= widget_button(b_toggle_ct, value='Auto Colortable', uname='b_auto_ct')
	;widget_control, b_auto_ct, /set_button


	; output
	;tmp_base	= widget_base  (type_base(0), column=2, /grid_layout, xpad=0)
	;b_output	= widget_base  (tmp_base, column=1, /nonexclusive)
	;t_output	= widget_button(b_output, value='redirect output to file: ', uname='save_2D_output')
	;t_ofile		= widget_text  (tmp_base, /editable, uname='2D_output_file', sensitive=0)
	;;l_otype		= widget_label (tmp_base, value='as ', /align_left)
	;;otype_strs	= ['eps', 'ps', 'text']
	;;cb_otype	= widget_combobox (tmp_base, value=otype_strs, uname='output_type')


; radial profile ---------------------------------------------------------------
	tmp_base	= widget_base (type_base(1), row=4, frame=1, xpad=0, ypad=0, space=0)

	; select profile combobox
	tmp_base1	= widget_base (tmp_base, column=2, /grid_layout, frame=0, xpad=0, ypad=0, space=0)
	l_1D_prof	= widget_label(tmp_base1, value='Select profile type: ')
	type_strs	= get_1D_plot_types()
	cb_1D_prof	= widget_combobox (tmp_base1, value=type_strs, uname='load_1D_id', xsize=160)

	; profile description
	tmp_base2	= widget_base (tmp_base, column=1, frame=0, xpad=0, ypad=0, space=0)
	l_1D_txt	= widget_label(tmp_base2, value='Description:', /align_left, resource_name='frame_title')
	t_1D_txt	= widget_text (tmp_base2, uname='1D_profile_text', xsize=58, ysize=4, /editable)

	; new profile configuration
	tmp_base3	= widget_base (tmp_base, row=2, /grid_layout, frame=0, xpad=0, ypad=0, space=0)
	l_R		= widget_label(tmp_base3, value='[R_start, R_end]: ', uname='l_1D_R')
	t_R1		= widget_text (tmp_base3, value='0.0', /editable, uname='t_1D_R1', xsize=10)
	t_R2		= widget_text (tmp_base3, value='0.0', /editable, uname='t_1D_R2', xsize=10)
	l_Z		= widget_label(tmp_base3, value='[Z_start, Z_end]: ', uname='l_1D_Z')
	t_Z1		= widget_text (tmp_base3, value='0.0', /editable, uname='t_1D_Z1', xsize=10)
	t_Z2		= widget_text (tmp_base3, value='0.0', /editable, uname='t_1D_Z2', xsize=10)

	tmp_base4	= widget_base (tmp_base, row=2, /grid_layout, frame=0, xpad=0, ypad=0, space=0)
	l_tangle	= widget_label(tmp_base4, value='Toroidal position [deg]: ', uname='lrad_tangle')
	t_tangle	= widget_text (tmp_base4, value='0.0', /editable, uname='rad_tangle')
	;l_pangle	= widget_label(tmp_base4, value='poloidal angle: ', uname='lrad_pangle')
	;t_pangle	= widget_text (tmp_base4, value='0.0', /editable, uname='rad_pangle')
	l_save_1D	= widget_label(tmp_base4, value='Save under name: ', uname='lsave_1D')
	t_save_1D	= widget_text (tmp_base4, value='', /editable, uname='save_1D')
;-------------------------------------------------------------------------------


; fieldline profile --------------------------------------------
; --- presently not supported ---
;	tmp_base	= widget_base (type_base(2), column=2, frame=1)
;	b_fl		= widget_button(tmp_base, value='load fieldline', uname='import_fl', resource_name='warning')
;	select_fl	= widget_text (tmp_base, /editable, uname='select_fl')

	return, base
end; make_plot_options_base

pro clear_drange
	id		= widget_info(1L, find_by_uname='dmin')
	widget_control, id, set_value=''
	id		= widget_info(1L, find_by_uname='dmax')
	widget_control, id, set_value=''
end; clear_drange
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
function make_data_base, top_base
	;common	event_id_cb
	common	config_cb

	base		= widget_base(top_base, column=2, frame=1)
	left_base	= widget_base(base, row=2, xpad=0)
	label		= widget_label(left_base, value='select volume data:', /align_left, resource_name='frame_title')


	tab		= widget_tab(left_base)

	wx_size		= 640
	wy_size		= 240
	; plasma data
	pl_base		= widget_base(tab, column=4, title='main plasma', /grid_layout, $
				/scroll, x_scroll_size=wx_size, y_scroll_size=wy_size)
	n_pl		= (size(plasma_arr, /dimensions))[1]
	pl_button	= make_array(n_pl, /int)
	for i=0,n_pl-1 do begin
		uname		= 'pl_id.'+strcompress(string(i), /remove_all)
		pl_button[i]	= widget_button(pl_base, value=plasma_arr[0,i], uname=uname)
	endfor


	; magnetic data
	mg_base		= widget_base(tab, column=3, title='magnetic geometry', /grid_layout, $
				/scroll, x_scroll_size=wx_size, y_scroll_size=wy_size)
	n_mg		= (size(magnetic_arr, /dimensions))[1]
	mg_button	= make_array(n_mg, /int)
	for i=0,n_mg-1 do begin
		uname		= 'mg_id.'+strcompress(string(i), /remove_all)
		mg_button[i]	= widget_button(mg_base, value=magnetic_arr[0,i], uname=uname)
	endfor


	; diagnostic data
	dg_base		= widget_base(tab, column=3, title='diagnostics', /grid_layout, $
				/scroll, x_scroll_size=wx_size, y_scroll_size=wy_size)
	n_dg		= (size(diagnostic_arr, /dimensions))[1]
	dg_button	= make_array(n_dg, /int)
	for i=0,n_dg-1 do begin
		uname		= 'dg_id.'+strcompress(string(i), /remove_all)
		dg_button[i]	= widget_button(dg_base, value=diagnostic_arr[0,i], uname=uname)
	endfor


	; impurity data
	imp_base	= widget_base(tab, column=1, title='impurities', xpad=0, ypad=0, /grid_layout, uname='imp_tab', $
				/scroll, x_scroll_size=wx_size, y_scroll_size=wy_size)
	update_impurity


	; data processing
	;data_proc_base	= make_data_proc_base(left_base)


	; plot options
	plt_base	= make_plot_options_base(base)	; (right_base)

	return, base
end; make_data_base
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro update_impurity
	common	config_cb
	common	case_info_cb

	; destroy old base
	;old_base	= widget_info (1L, find_by_uname='imp_base')
	;if (old_base ne 0) then widget_control, old_base, /destroy
	;if (niont le 1) then return

	top_base	= widget_info (1L, find_by_uname='imp_tab')
	new_base	= widget_base(top_base, column=1, uname='imp_base', xpad=0, ypad=0)
	;label		= widget_label(new_base, value='impurities')


	;top_base	= widget_info(1L, find_by_uname='impurity_tab')
	imp_base1	= widget_base(new_base, column=2, /grid_layout)
	imp_base2	= widget_base(new_base, column=4, /grid_layout)
;
	n_imp		= (size(impurity_arr, /dimensions))[1]
	imp_button	= make_array(n_imp, /int)
;	imp_strs	= make_array(NPSPEI, /string)
;	k		= 0
;	for i=0,niont-1 do begin
;	for j=0,nstage(i)-1 do begin
;		imp_strs[k]	= strcompress(text_ion(i)) + ' '+strcompress(string(j+1), /remove_all) + '+ '
;		k	= k + 1
;	endfor
;	endfor

	;print, 'NPSPEI = ', NPSPEI
	for i=0,n_imp-1 do begin
		uname		= 'imp_id.'+strcompress(string(i), /remove_all)
		imp_button[i]	= widget_button(imp_base2, value=impurity_arr[0,i], uname=uname)
	endfor
	isel_label	= widget_label(imp_base1, value='select impurity ion species')
	;isel_dlist	= widget_droplist(imp_base1, value=imp_strs, uname='imp_select_id')
	isel_dlist	= widget_droplist(imp_base1, value='[]', uname='imp_select_id')
end; update_impurity
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro update_zooms
	common	config_cb


	new_machine	= get_selected_machine()
	id	= widget_info(1L, find_by_uname='cb_zoom_predef')
	iarr	= where(zooms[*].machine EQ new_machine, count)
	zoom_str = 'manual'
	zoom_id  = -1
	if (count GT 0) then begin
		zoom_str = [zoom_str, zooms[iarr].tag]
		zoom_id  = [zoom_id, iarr]
	endif
	widget_control, id, set_value=zoom_str
end; pro update_zooms
;-----------------------------------------------------------------------
pro select_zoom
	common	config_cb

	id	= widget_info(1L, find_by_uname='cb_zoom_predef')
	text	= widget_info(id, /combobox_gettext)

	;machine	= get_selected_machi
	;print, 'zoom_id = ', zoom_id
	idx	= where(text eq zoom_str)
	idz	= zoom_id(idx(0))
	print, 'idz = ', idz

	if (idz ge 0) then begin
		xmin	= strcompress(string(zooms(idz).Rmin), /remove_all)
		xmax	= strcompress(string(zooms(idz).Rmax), /remove_all)
		ymin	= strcompress(string(zooms(idz).Zmin), /remove_all)
		ymax	= strcompress(string(zooms(idz).Zmax), /remove_all)
		phi	= strcompress(string(zooms(idz).Zmax), /remove_all)
	endif else begin
		xmin	= ''
		xmax	= ''
		ymin	= ''
		ymax	= ''
		phi	= '0.1'
	endelse

	set_entry, 'xmin', xmin
	set_entry, 'xmax', xmax
	set_entry, 'ymin', ymin
	set_entry, 'ymax', ymax
	set_entry, 'rad_tangle', phi
end; pro select_zoom
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
function make_depo_base, top_base
	common	config_cb
	base		= widget_base(top_base, column=1)
	base_label	= widget_label(base, value='select target data:', /align_left, resource_name='frame_title')
	tab		= widget_tab(base)

	; deposition data
	dp_base		= widget_base(tab, row=4, title='deposition data', /grid_layout)
	n_dp		= (size(depo_arr, /dimensions))[1]
	dp_button	= make_array(n_dp, /int)
	for i=0,n_dp-1 do begin
		uname		= 'dp_id.'+strcompress(string(i), /remove_all)
		dp_button[i]	= widget_button(dp_base, value=depo_arr[0,i], uname=uname)
	endfor

	; select limiter
	lim_base	= widget_base(tab, row=3, title='select output', /grid_layout)
	lim_label	= widget_label(lim_base, value='limiter: ')
	empty_str	= ''
	lim_select	= widget_droplist(lim_base, value=empty_str, uname='lim_select_id', scr_xsize=480)

	; select profiles
	lx_profile	= widget_label(lim_base, value='Select x-profile at y =')
	ex_profile	= widget_text(lim_base, value='', /editable, uname='target_xid')
	ly_profile	= widget_label(lim_base, value='Select y-profile at x =')
	ey_profile	= widget_text(lim_base, value='', /editable, uname='target_yid')

	return, base
end; make_depo_base
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
function make_quit_base, top_base
	common	event_id_cb

	;base	= widget_base(row=1)
	button	= widget_button(top_base, value='Quit', uname=quit_id, resource_name='quit')

	;return, base
	return, button
end; function make_quit_base
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro quit, event_id
	common	config_cb

	; save history
	history	= getenv('HOME')+'/.emc3_display/history'
	openw, lun, history, /get_lun
	printf, lun, get_selected_machine()
	printf, lun, get_selected_shot()
	printf, lun, get_selected_case()
	printf, lun, get_selected_run()
	free_lun, lun

	;id	= widget_info(1L, find_by_uname='plot_window')
	;widget_control, id, /destroy

	;widget_control, overlay_id, /destroy
	; destroy main window
	widget_control, event_id, /destroy
end; pro quit
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro main_window_event, event
	common	config_cb
	common	event_id_cb

	event_id	= widget_info(event.id, /uname)
	main_id		= event_id

	; select sublevel id (if exists)
	i		= strpos(event_id, '.')
	if (i ne -1) then begin
		main_id = strmid(event_id,0,i)
		sub_id	= long(strmid(event_id,i+1,strlen(event_id)-i))
	endif

	case main_id of
	quit_id:	quit, event.top
	cb_machine_id:	update_machine
	cb_shot_id:	update_shot
	cb_case_id:	update_case
	cb_run_id:	update_run
	'cb_cmp_case':	update_cmp_case
	;'plot_button':	plot_test
	'pl_id':	plot_data, 0, sub_id
	'mg_id':	plot_data, 2, sub_id
	'dg_id':	plot_data, 3, sub_id
	'imp_id':	plot_data, 4, sub_id, species_id=get_select('imp_select_id')+1
	;mg_id:		mg_select, sub_id
	'dp_id':	plot_depo_data, sub_id, limiter=get_select('lim_select_id');			plot deposition data
	cb_plt_type_id:	set_plot_option, event.index, /coord
	plot_grid_id:	set_plot_option, widget_info(event.id, /button_set), /plot_grid
	;animated_id:	set_plot_option, widget_info(event.id, /button_set), /animated
	'save_output':	toggle_sensitive, event_id, 'output_file'
	'set_range_id':	begin
				toggle_sensitive, event_id, 'xmin'
				toggle_sensitive, event_id, 'xmax'
				toggle_sensitive, event_id, 'ymin'
				toggle_sensitive, event_id, 'ymax'
				toggle_sensitive, event_id, 'cb_zoom_predef'
			end
	'cb_zoom_predef':	select_zoom
	'compare_runs':	begin
				toggle_sensitive, event_id,  'l_cmp_case'
				toggle_sensitive, event_id, 'cb_cmp_case'
				toggle_sensitive, event_id,  'l_cmp_run'
				toggle_sensitive, event_id, 'cb_cmp_run'
				toggle_sensitive, event_id, 'cb_cmp_diff'
			end
	'b_select_ct':	begin
				xloadct, /block, file=root_dir+'/'+CT_file
			end
	'b_modify_ct':	xpalette, /block
	'b_select_overlay_data':		overlay_data_ui
	'b_clear_drange': 	clear_drange
	;'import_fl':	load_fieldline_ui
	'import_fl':	load_fieldlines
	'load_1D_id':	update_1D_plot_window
	else:
	endcase
	
end; pro main_window_event
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro main_window
	common	config_cb
	; create and fill main window
	base		= widget_base(column=1, title='emc3_display', resource_name='emc3_display')

	run_base	= make_run_base(base)
	settings_base	= make_settings_base(base)
	data_base	= make_data_base(base)
	depo_base	= make_depo_base(base)
	quit_base	= make_quit_base(base)
	check_data

        widget_control, base, /realize
	base_id		= base

        xmanager, 'main_window', base
end; pro main_window
;-----------------------------------------------------------------------
