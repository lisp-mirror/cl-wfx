

function before_ajax(context)
{
/*
    $("textarea:not('.no-mce')", context).each(function () {
        tinymce.execCommand('mceRemoveEditor', false, $(this).attr("id"));
    });
*/
}



function fileUploadPrep (args) {

        $(".file-upload").each(function (i,file) {

	    var args = JSON.parse($("#args-" + file.id).val());
	    //doing fucked up concat because "" + lic drops leeading 0's even
	    //though type() syste license is a string????
	    var fuck = "/cor/file-upload?license=";
	    var license = args.license.toString();
	    
	   // alert($("#init-" + file.id).val() );
	    
            $("#" + file.id).fileinput({
		uploadUrl: fuck.concat(license)
		    + "&collection=" + args.collection
		    + "&datatype=" + args.datatype
		    + "&field=" + args.field + "",
		uploadAsync: false,
		theme: "fa",
		//overwriteInitial: false,
		initialPreviewAsData: true,
		initialPreview: [$("#init-" + file.id).val() ],
		//uploadExtra: {args: JSON.stringify(args)},
		maxFileCount: 1
	    }).on('filebatchuploadsuccess', function(e, params) {
		//console.log('file uploaded', e, params);
		//alert(params.files[0].name);
		$("#init-" + file.id).val(params.files[0].name.toLowerCase());
	    });

    });
}

function applyPeach (context)
{

     $('.wfx-script').each(function(i,textarea) {
	
     	 editor = CodeMirror.fromTextArea(textarea, {
             lineNumbers: true,
             smartIndent: true,
             autoCloseBrackets: true,
 	     showTrailingSpace: true,
	     matchBrackets: true,
             mode: "text/x-common-lisp"});
	 editor.display.wrapper.style.fontSize = "12px";
         editor.refresh();});


    
   /* $('.date', context).datepicker({format: 'dd M yyyy'});

    $("textarea:not('.no-mce')", context).each(function () {

        var id = $(this).attr("id");
        if (!id) {
            id = "textarea" + tmp_counter++;
            $(this).attr("id", id);
        }

        var plugins = [];
        if ($(this).hasClass('image'))
            plugins = ["image","code"];

        tinymce.init({
            selector: "#"+id,
            plugins: plugins,
            toolbar: " undo redo | styleselect | bold italic | alignleft aligncenter alignright alignjustify | bullist numlist outdent indent | link image | code",
            file_picker_callback: function(callback, value, meta) {
                $("body").remove("#invisible_div");
                $("body").append('<div id="invisible_div" style="display:none"><input id="mce_upload" type="file" name="image-upload"</div>');
                $('#mce_upload').fileupload({url: '/insite/ajax/IMAGE-UPLOAD',
                                             done:
                                             function(e, data) {
                                                 $("body").remove("#invisible_div");
                                                 callback(data.result);
                                             }});
                $('#mce_upload').click();
            }
        });
    });

*/}



function fetchURI(uri, callback, parameters) {
    var request;
    if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
    else {
        try { request = new ActiveXObject("Msxml2.XMLHTTP"); } catch (e) {
            try { request = new ActiveXObject("Microsoft.XMLHTTP"); } catch (ee) {
                request = null;
            }}}
    if (!request) alert("Browser couldn't make a request object.");

    request.open('POST', uri, true);
    request.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
//alert(callback);
    request.onreadystatechange = function() {
        if (request.readyState != 4) return;
        if (request.status >= 200 && request.status < 300
            || request.status == 304) {
            if (callback)
                callback(request.responseText);
        }
        else {
            alert('Error while fetching URI ' + uri);
        }
    };
    request.send(parameters);

    delete request;
}


function cl_ajax_render (script_name, renderer, id, args, callback) {
    ajax_call('CL-AJAX-RENDER', callback, [script_name, renderer , id], args);
}

function find_widget(id) {
    var widget = document.getElementById(id);

    if (!widget) {
        alert("find_widget: There's no widget with id " + id);
        return;
    }
    return widget;
}

function ajax_render_event_key(script_name,renderer,source_id,key,id,args){

    if (event.which == key){	
	var edValue = document.getElementById(source_id);
	var arr = [];

	arr.push([source_id,edValue.value]);
	for (i = 0; i < args.length; ++i) {
		     arr.push(args[i]);
		 }

        ajax_render(script_name,renderer,id, arr);
    }
}

function ajax_render (script_name, renderer, id, args) {
    var widget = find_widget(id);
/*    jQuery.fallr('show', {
        buttons     : {},
	content     : 'Loading data',
	icon        : 'info',
	useOverlay  : false
    });*/

    
    if (renderer) {
	args.push(['context-uri',script_name]);
        cl_ajax_render(script_name, renderer ,id, args,
                       function (response) {
//alert("fuck");
                           var json = jQuery.parseJSON(response);
//alert(json);
                           before_ajax(widget);
                           widget.innerHTML = json[0];
                           applyPeach(widget);

			   fileUploadPrep(args);
			   
                           if (json[1]) {
                               eval(json[1]);
                           }
                           //jQuery.fallr('hide');
                       }
                      );
    }
}

function get_values(widget, tag_name, disabled) {
    var elements = widget.getElementsByTagName(tag_name);
    var result = [];
   
    for (var i = 0; i < elements.length; i++) {
        element = elements[i];

        if (element.name && (disabled || !element.disabled))
        {
            if (tag_name == 'input')
            {
                if (element.type == 'checkbox')
                {
                    result.push([element.name, [element.checked,element.value]]);
                }
                else if (element.type == 'radio')
                {
                    if (element.checked)
                        result.push([element.name, element.value]);
                }
                else
                {
                    result.push([element.name, element.value]);
                }
            } else if (element.id && tag_name == 'textarea') {
                result.push([element.name, element.value]);
            }
            else
            {
                result.push([element.name, element.value]);
            }
        }
    }
    return result;
}


var scripts = [];

function save_scripts() {

   for (i = 0; i < scripts.length; i++) {
     scripts[i].save();
    }
}

function add_scripts(script) {
   scripts.push(script);
}

function get_form_values(form_id, disabled) {
    var widget = find_widget(form_id);
    var input_types = ['input', 'select', 'textarea'];

    
    if (widget) {
        //tinyMCE.triggerSave();
        save_scripts();
        var result = [];
        for (var i = 0; i < input_types.length; i++){
            result = result.concat(get_values(widget, input_types[i], disabled));
	}
        return result;
    }
}
