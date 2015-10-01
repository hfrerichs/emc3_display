;=======================================================================
; module:	overlay_data
; date:		Aug 17th, 2007
;
; author:	Heinke Frerichs
; email:	h.frerichs@fz-juelich.de
;
; description:	User interface to select data for overlaying
;		(poincare plot, contour line, ...)
;=======================================================================



;=======================================================================
; user interace and event handler
pro overlay_data_event, event
	common	config_cb

	event_id	= widget_info(event.id, /uname)
	if (event_id eq 'ov_close_id') then begin
		id		= widget_info(overlay_id, find_by_uname='tb_overlay')
		widget_control, id, get_value=tb_overlay_data

		widget_control, event.top, /destroy
		overlay_id	= 0
		id              = widget_info(1L, find_by_uname='b_set_overlay')
		widget_control, id, /set_button
	endif
	if (event_id eq 'ov_add_id') then begin
		id		= widget_info(overlay_id, find_by_uname='tb_overlay')
		widget_control, id, insert_rows=1
	endif
	if (event_id eq 'ov_del_id') then begin
		id		= widget_info(overlay_id, find_by_uname='tb_overlay')
		widget_control, id, delete_rows=1
	endif
end; overlay_data_event
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
pro overlay_data_ui
	common	config_cb

	if (overlay_id ne 0) then return
	base		= widget_base(column=1, title='overlay data', frame=1, resource_name='emc3_display')

	cl		= ['filename', 'x column', 'y column', 'color', 'fill-color', 'style', 'line width']
	cw		= [256, 64, 64 , 64, 64, 64, 64]
	if (size(tb_overlay_data, /type) eq 3) then begin
		data		= {filename:'', xcolumn:0, ycolumn:1, color:255, fillcolor:-1, style:0, linewidth:1}
	endif else begin
		data	= tb_overlay_data
	endelse
	table		= widget_table(base, scr_ysize=160, value=data, column_labels=cl, column_widths=cw, /editable, uname='tb_overlay')

	add_button	= widget_button(base, value='Add row',   uname='ov_add_id')
	del_button	= widget_button(base, value='Delete row',   uname='ov_del_id')
	close_button	= widget_button(base, value='Close', uname='ov_close_id', resource_name='Green')

	widget_control, base, /realize
	overlay_id	= base
	xmanager, 'overlay_data', base
end; overlay_data_ui
;=======================================================================



;=======================================================================
; actual overlaying of data
pro overlay_data, poly_test
	common	config_cb

	; return if overlaying data is turned off
	if not check_button('b_set_overlay') then return

	; return if no overlay data is defined
	if ((size(tb_overlay_data))[0] eq 0) then return

	n	= (size(tb_overlay_data))[1]
	for i=0,n-1 do begin
		filename	= tb_overlay_data[i].filename
		if (filename ne '') then print, 'overlaying data from file: ', tb_overlay_data[i].filename

		; read data
		if not query_ascii (filename, info) then continue
		data	= read_ascii (filename, comment_symbol='#', delimiter=' ')

		; check if data is in row/column format
		if ((size(data.field1))[0] ne 2) then begin
			print, 'data not in row/column format!'
			continue
		endif

		n	= (size(data.field1, /dimensions))[0]
		m	= (size(data.field1, /dimensions))[1]

		; check column number of x- and y-data is valid
		ix	= tb_overlay_data[i].xcolumn
		iy	= tb_overlay_data[i].ycolumn
		if (ix ge n) then begin
			print, 'x-column # ', ix, ' greater than total # of columns: ', n
			continue
		endif
		if (iy ge n) then begin
			print, 'y-column # ', iy, ' greater than total # of columns: ', n
			continue
		endif

		; plot data
		color	= tb_overlay_data[i].color
		fcolor	= tb_overlay_data[i].fillcolor
		psym	= tb_overlay_data[i].style
		lw  	= tb_overlay_data[i].linewidth
		xdata	= data.field1(ix,*)
		;ydata	= 360 - data.field1(iy,*)
		ydata	= data.field1(iy,*)

		if (fcolor ne -1) then polyfill, xdata, ydata, color=fcolor, noclip=0, thick=lw
		oplot, xdata, ydata, color=color, psym=psym, thick=lw

		poly_test	= data.field1
	endfor
end; overlay_data
;=======================================================================
