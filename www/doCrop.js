$(document).ready(function() {	
	
	$("#crop").on("click",function(){			
		// Shiny.onInputChange("dimensions2",$("#crop").text()); 	
		var str = "";
		$('.geometry_label_box').each(function(){
		  str += $(this).text();
		});
		Shiny.onInputChange("dimensions", str);
		Shiny.onInputChange("iniateCrop",Math.random());
	});
	
});