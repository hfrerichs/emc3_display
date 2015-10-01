;-----------------------------------------------------------------------
; module:	settings
; date:		Apr 03rd, 2008
;
; author:	Heinke Frerichs
; email:	hfrerichs@wisc.edu
;
; description:	user interface for plot settings
;-----------------------------------------------------------------------



;=======================================================================
; user interface for redirecting output
function make_output_base, top_base

	base		= widget_base(top_base, row=2, /grid_layout, frame=1, xpad=0, ypad=0)
	b_output	= widget_base  (base, column=1, /nonexclusive)
	t_output	= widget_button(b_output, value='save output to file: ', uname='save_output')
	t_ofile		= widget_text  (base, /editable, uname='output_file', sensitive=0)

        ; pre-defined formats for eps-output
	l_load		= widget_label (base, value='select print format: ')
	eps_formats	= ['1x2', '1x1', '5x4', '1x2 (no boundary)']
	cb_load		= widget_combobox (base, value=eps_formats, uname='cb_eps_formats')

	return, base
end; make_output_base
;=======================================================================



;=======================================================================
; user interface for colortables
function make_colortab_base, top_base
	base		= widget_base  (top_base, column=3, frame=1, /grid_layout, xpad=0, ypad=0)
	b_select_ct	= widget_button(base, value='Select Colortable', uname='b_select_ct', resource_name='ct_button')
	b_modify_ct	= widget_button(base, value='Modify Colortable', uname='b_modify_ct')
	b_toggle_ct	= widget_base  (base, column=1, /nonexclusive, xpad=1, ypad=1)
	b_auto_ct	= widget_button(b_toggle_ct, value='Auto Colortable', uname='b_auto_ct')
	widget_control, b_auto_ct, /set_button

	return, base
end; make_colortab_base
;=======================================================================



;=======================================================================
; user interface for data processing
function make_data_proc_base, top_base
	base		= widget_base (top_base, column=1, frame=1, xpad=0, ypad=0) 

	; scaling
	sc_base		= widget_base (base, column=2, /grid_layout, xpad=0, ypad=0)
	sc_label	= widget_label(sc_base, value='scale factor: ')	; scale data,	factor:
	sc_data		= widget_text (sc_base, value='1.0', /editable, uname='sc_factor')

	; offset
	; shift data,	offset:

	; transform data,	function

	; logarithmic plot
	log_base	= widget_base  (base, column=1, /nonexclusive, xpad=1, ypad=1)
	b_log		= widget_button(log_base, value='Logarithmic plot', uname='b_log_plot')
	;widget_control, b_log, /unset_button

	return, base
end; make_data_proc_base
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
function process_data, data

	; scaling
	str	= get_entry('sc_factor')
	if (str ne '') then begin
		scale_factor	= (double(str))[0]
		print, 'scale factor: ', scale_factor
		data(*)		= scale_factor * data(*)
	endif

	; logarithmic plot
	if (check_button('b_log_plot')) then begin
		data(*)	= alog10(data(*))
	endif

	return, data
end; process_data
;=======================================================================



;=======================================================================
; user interface for overlaying data
function make_overlay_data_base, top_base
	base		= widget_base  (top_base, column=2, frame=1, /grid_layout, xpad=0, ypad=0)

	toggle_base	= widget_base  (base, column=1, /nonexclusive, xpad=1, ypad=1)
	b_set_overlay	= widget_button(toggle_base, value='Overlay Data', uname='b_set_overlay')
	b_select_data	= widget_button(base, value='Select Data', uname='b_select_overlay_data', resource_name='LGreen')
	return, base
end; make_overlay_data_base
;=======================================================================



;=======================================================================
; user interface for window size and margins
function make_size_base, top_base
	hbase		= widget_base  (top_base, column=2, xpad=0, space=0)
	base1		= widget_base  (hbase, column=2, /grid_layout)
	base2		= widget_base  (hbase, row=3, frame=1, xpad=0, space=0, /grid_layout)

	l_load		= widget_label (base1, value='load predefined values from: ')
	cb_load		= widget_combobox (base1, value=['1','2'], uname='cb_load_size_and_margins')

	l_size		= widget_label (base2, value='Window size (X,Y): ')
	t_xsize		= widget_text  (base2, /editable, uname='win_xsize', xsize=10)
	t_ysize		= widget_text  (base2, /editable, uname='win_ysize', xsize=10)
	l_plmargin	= widget_label (base2, value='Plot margin (XL,XR): ')
	t_plXL		= widget_text  (base2, /editable, uname='plXL', xsize=10)
	t_plXR		= widget_text  (base2, /editable, uname='plXR', xsize=10)
	l_cbmargin	= widget_label (base2, value='Color bar margin (XL,XR): ')
	t_cbXL		= widget_text  (base2, /editable, uname='cbXL', xsize=10)
	t_cbXR		= widget_text  (base2, /editable, uname='cbXR', xsize=10)
end; make_size_base
;=======================================================================



;=======================================================================
function make_labels_base, top_base
	base		= widget_base  (top_base, row=2, frame=1, xpad=0, space=0, /grid_layout)

	l_title		= widget_label (base, value='Title: ')
	t_title		= widget_text  (base, /editable, uname='plot_title', xsize=20)
	l_units		= widget_label (base, value='Quantity [Units]: ')
	t_units		= widget_text  (base, /editable, uname='plot_units', xsize=20)
end; make_labels_base
;=======================================================================



;=======================================================================
; main frame for plot settings
function make_settings_base, top_base

	base		= widget_base(top_base, column=1, frame=1)
	label		= widget_label(base, value='common plot settings:', /align_left, resource_name='frame_title')

	tab		= widget_tab(base, uname='settings_tab')
	s_base		= make_array(6, /int)
	s_strs		= ['output', 'colors', 'data processing', 'overlay data', 'size and margins', 'labels']
	for i=0,5 do begin
		s_base(i)	= widget_base(tab, title=s_strs(i), column=1, frame=1)
	endfor

	output_base	= make_output_base(s_base(0))
	colortab_base	= make_colortab_base(s_base(1))
	data_proc_base	= make_data_proc_base(s_base(2))
	overlay_base	= make_overlay_data_base(s_base(3))
	size_base	= make_size_base(s_base(4))
	labels_base	= make_labels_base(s_base(5))

	return, base
end; make_settings_base
;=======================================================================
