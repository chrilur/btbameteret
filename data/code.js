var plotvalg = {series: 
						{marker: 
							{enabled: true, radius: 3
							}
						}
				};
var legende = {align: 'center',
				x: 0,
				y: 0,
				floating: true,
				borderWidth: 1,
				backgroundColor: '#FFFFFF',
				shadow: true
					};
var number = {formatter: function() 
              {return ''+this.series.name +'<br>Uke '+ this.x +': '+ this.y +' ';}
					};
var percent = {formatter: function() 
               {return ''+this.series.name +'<br>Uke '+ this.x +': '+ this.y +'%';}
              }	;
var notitle = {text: null};
var xaxisstandard = {style: {
                        color: 'black'},
				        align: 'center', x: 0, y: 10
				        };
var noscroll = {enabled: false}