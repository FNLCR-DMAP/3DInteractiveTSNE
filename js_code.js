  shinyjs.plot3d = function(mydiv){

    console.log(mydiv[1])

    var data = []
    
    if (mydiv[5]) {
    
      colsels = [
        '#1f77b4',
        '#ff7f0e',
        '#2ca02c',
        '#d62728',
        '#9467bd',
        '#8c564b',
        '#e377c2',
        '#7f7f7f',
        '#bcbd22',
        '#17becf'
      ];
      
      var cats = mydiv[4].filter((v,i,a) => a.indexOf(v)===i)
      console.log(cats)
      
      var catcols = {}
      var styledict = {}
      
      for (isl=0; isl<cats.length;isl++) {
        catcols[cats[isl]] = colsels[isl]
        styledict[cats[isl]] = {target: cats[isl], value: {marker: {color: colsels[isl], symbol: mydiv[7], size:mydiv[6]}}}
      }
      
      console.log(catcols)
      console.log(styledict)
      
      globalstyles = Object.values(styledict)
      console.log(globalstyles)
      
      data = [
        {
          opacity:1,
          type: 'scatter3d',
          mode: 'markers',
          
          x: mydiv[1],
          y: mydiv[2],
          z: mydiv[3],
          
          text: mydiv[4],
          hoverinfo:'text',
          transforms: [{
            type: 'groupby',
            groups: mydiv[4],
            styles: globalstyles
          }]
        }
      ];
    }
    else{
      data = [
        {
          opacity:1,
          type: 'scatter3d',
          mode: 'markers',
          
          x: mydiv[1],
          y: mydiv[2],
          z: mydiv[3],
          
          text: mydiv[4],
          hoverinfo:'text',
          
          marker: {
            size: mydiv[6],
            symbol: mydiv[7],
            color: mydiv[4],
            colorbar: {thickness:20}
          }
        }
      ];
    }
    
    var camera= {
      'eye': {'x':0, 'y':1, 'z':0},
      'up': {'x':0, 'y':0, 'z':1},
      'center': {'x':0, 'y':0, 'z':0},
    }
    
    var gd3D = document.getElementById('mydiv');
    
    if(gd3D.className !== 'myplot') {
      camera=gd3D._fullLayout.scene.camera;
    }
    
    var layout = {
      autosize: false,
      width:600,
      height:600,
      scene: {
        aspectmode: 'manual',
        aspectratio: {
          x:1, y:1, z:1,
        },
        xaxis: {
          nticks: 5,
          autorange: true,
        },
        yaxis: {
          nticks: 5,
          autorange: true,
        },
        zaxis: {
          nticks: 5,
          autorange: true,
        },
        camera: camera
      }
    };
    
    var graphDiv3D = document.getElementById(mydiv[0])
    
    Plotly.newPlot(graphDiv3D,data,layout)
    
  }