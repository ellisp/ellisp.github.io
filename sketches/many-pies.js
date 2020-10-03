

//-----------------------Data and global variables-----------------
// Australian value add by industry division, top 7 divisions and 'other'
// Sourced from https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/8155.02018-19?OpenDocument
let the_data = [ 51983, 46153, 32061, 25092, 16001, 14240, 13992, 61200 ];

// Calculate total sum of the_data so can use it for scaling:
let sum = the_data.reduce(function(a, b){
    return a + b;
}, 0);
 
// Rescale the data from 0 to 360 (for degrees):
for(let i =0; i < the_data.length; i++){
	the_data[i] = the_data[i] / sum * 360
} 
    
let pies = new Array();

// Brewer colours, Set1 palette
//let palr = [213,  244,  253,  254,  230,  171,  102,   50];
//let palg = [62,  109,  174,  224,  245,  221,  194,  136];
//let palb = [79,   67,   97,  139,  152,  164,  165,  189];

// Brewer colours, Set3 palette
let palr = [141, 255, 190, 251, 128, 253, 179, 252];
let palg = [211, 255, 186, 128, 177, 180, 222, 205];
let palb = [199, 179, 218, 114, 211, 98, 105, 229];

//--------------------------Main sketch program ('setup()' and 'draw()')-------------------
function setup() {
    // create a canvas the width of the div, and movee it to 
    // inside the div with id='sketch-holder':
   // var thisdiv = document.getElementById('sketch-holder')
    var canvas = createCanvas(Math.min(windowWidth, 680), 400);
    canvas.parent('sketch-holder');
      
    noStroke();
    frameRate(60);
      
	// Create 200 pie objects, in the centre of the canvas:
	for(let i = 0; i < 200; i++){
	  
		pies.push(new PieChart(the_data, random(1, width / 4), 0, 0, 0, 
							   randomGaussian() * 0.03, random(20, 230)));						   
	}
}

// Main loop
function draw() {
	background(0);
	for(i = 0; i < pies.length; i++){
		pies[i].spin();  
		pies[i].move(0.2);
		pies[i].resize(2);
		pies[i].display(); 
	}
	
    // If mouse pressed, re-create all the pies centred in the mouse location.
    // Half of these will be a new, small size; half will be whatever size they
    // were previously:
    if (mouseIsPressed){
        for(let j = 0; j < pies.length; j++){
            if(random(0,1) > 0.5){
                pies[j].relocate(mouseX - width/2, mouseY - height/2, 10);
            } else {
                pies[j].relocate(mouseX - width/2 ,mouseY - height/2, pies[j].diameter);
            }
        }
    }
}

//---------------PieChart objects and their methods----------------------------
class PieChart {
 
    constructor(temp_data, temp_diameter, temp_startAngle, 
                temp_x, temp_y, temp_ss, temp_alpha){
        this.data = temp_data;
        this.diameter = temp_diameter;
        this.startAngle = temp_startAngle;
        this.x = temp_x;
        this.y = temp_y;
        this.xv = 0;
        this.yv = 0;
        this.ss = temp_ss;
        this.alpha = temp_alpha;
        this.aspect = random(0.5, 1.5)
    }
     
    display() {

        push();
        translate(this.x, this.y);

        let segmentAngle = this.startAngle

        // Draw the pie, one wedge at a time
        for (let i = 0; i < this.data.length; i++) {

        fill(palr[i], palg[i], palb[i], this.alpha);
        arc(width/2, 
            height/2, 
            this.diameter * this.aspect, 
            this.diameter, 
            segmentAngle, 
            segmentAngle + radians(this.data[i]));
        segmentAngle += radians(this.data[i]); 
        }
        pop();
    }
 
    // spin method
    spin() {
        this.startAngle += this.ss;
    }
 
    // move method  with a speed argument supplied:
    move(sp) {
  
        // change speeds: 
        this.xv += randomGaussian() * sp;
        this.yv += randomGaussian() * sp;

        // if speed too high in one direction, or off screen, accelerate in other direction:
        let v_constraint = 15;
        if(this.xv > sp * v_constraint | this.x > width){
            this.xv -=sp;
        }
        if(this.xv < -sp * v_constraint | this.x < (-width)){
            this.xv += sp;
        }
        if(this.yv > sp * v_constraint | this.y > height){
            this.yv -= sp;
        }
        if(this.yv < -sp * v_constraint | this.y < (-height)){
            this.yv += sp;
        }

        // Change location based on the speed
        this.x += this.xv;
        this.y += this.yv;
    }
 
    // resize method with a scaling for how much to resize by (roughly):
    resize(z) {
        this.diameter += randomGaussian() * z;
        if(this.diameter < (z + 1)){
            this.diameter = 2 * z ; 
        }
    }
 
    // relocate to new position and diameter:
    relocate(newx, newy, newd) {
        this.x = newx;
        this.y = newy;
        this.diameter = newd;
    }
}
