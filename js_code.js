shinyjs.plot3d = function(projectionDiv){
    console.log(projectionDiv[1])

    var data = []
    
    if (projectionDiv[5]) {
    
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
      
      var cats = projectionDiv[4].filter((v,i,a) => a.indexOf(v)===i)
      console.log(cats)
      
      var catcols = {}
      var styledict = {}
      
      for (isl=0; isl<cats.length;isl++) {
        catcols[cats[isl]] = colsels[isl]
        styledict[cats[isl]] = {target: cats[isl], value: {marker: {color: colsels[isl], symbol: projectionDiv[7], size:projectionDiv[6]}}}
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
          
          x: projectionDiv[1],
          y: projectionDiv[2],
          z: projectionDiv[3],
          
          text: projectionDiv[4],
          hoverinfo:'text',
          transforms: [{
            type: 'groupby',
            groups: projectionDiv[4],
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
          
          x: projectionDiv[1],
          y: projectionDiv[2],
          z: projectionDiv[3],
          
          text: projectionDiv[4],
          hoverinfo:'text',
          
          marker: {
            size: projectionDiv[6],
            symbol: projectionDiv[7],
            color: projectionDiv[4],
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
    
    var gd3D = document.getElementById('projectionDiv');
    
    if(gd3D.className !== 'myplot') {
      camera=gd3D._fullLayout.scene.camera;
    }
    
    var layout = {
      autosize: false,
      width:800,
      height:800,
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
    
    var graphDiv3D = document.getElementById(projectionDiv[0])
    
    Plotly.newPlot(graphDiv3D,data,layout)
    
  }
  
  