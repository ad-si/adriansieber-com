<?php

header("Content-Type: image/svg+xml");

function create_circles()
{
	
		$cont .='
		<svg id="logo" xmlns="http://www.w3.org/2000/svg" xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" 
		width="20" height="60" >
			<g transform="translate(0, 50)" >';
			for($i = 0; $i <6; $i++){
			$cont .= '
				<a xlink:href="index.php">
					<text style="font:900 60px/40px Trebuchet MS; opacity:'.(rand(0,9) * 0.1).';"
					fill="rgb('.rand(1,255).', '.rand(100,255).', '.rand(240,255).')" stroke="white" stroke-width="2"
						x="0" y="0" transform="translate('.rand(0,8).', '.rand(0,8).')" >Vectual
					</text>
				</a>';
			}
			$cont .= '
			</g>
		</svg>';
    echo $cont;
}	

create_circles();	
	
?>