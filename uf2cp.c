/*****************************************************************
 * project : uf2txt.c                                            *
 * comfile : gcc -o  uf2txt uf2txt.c                       \     *
 *            -I/usr/local/trmm/GVBOX/include              \     *
 *            -L/usr/local/trmm/GVBOX/lib -lrsl -lm              *
 *****************************************************************/
#include "rsl.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

//#define cell_size 1.0   // 1.0km

/*static char *RSL_ftype[] =
 {"DZ", "VR", "SW", "CZ", "ZT", "DR",
  "LR", "ZD", "DM", "RH", "PH", "XZ",
  "CD", "MZ", "MD", "ZE", "VE", "KD",
  "TI", "DX", "CH", "AH", "CV", "AV",
  "SQ","AT","BK"};*/
    
void print_header_for_radar(Radar  *radar);
void print_header_for_volume(Volume *v);
void print_header_for_sweep(Sweep *s);
void print_header_for_ray(Ray *ray);
void print_ray_values(Ray *ray);

int uf2cp_(char *filename, int *ny_max, int *nx_max, float *grid_resol, int *var_name, float var[*nx_max][*ny_max], int *ny, int *nx)
{
  Radar  *radar;
  Volume *volume;
  Cappi  *cappi;
  Carpi  *carpi;
  Carpi  *carpi2;

  //char sname[120];
  int i, j, n;
  int RayCount,BinCount;
  float ht;    //Cappi
  float range, range2; 
  float Z, temp1, temp2;
  float xl,yl;
  float aws_x, aws_y;
  int INDEX;


   for(i=0;i<256;i++){ if(filename[i]==' '){filename[i]='\0';break;} }  

  //radar = RSL_anyformat_to_radar(argv[1], NULL);
  radar = RSL_anyformat_to_radar(filename, NULL);
  //volume = radar->v[INDEX];
  
  ht = (1500.0 - radar->h.height)/1000.; //1500 -> m단위 고도
  INDEX = *var_name;
  range = (radar->v[INDEX]->sweep[0]->ray[0]->h.nbins * 
           radar->v[INDEX]->sweep[0]->ray[0]->h.gate_size + 
           radar->v[INDEX]->sweep[0]->ray[0]->h.range_bin1)/1000;

  int NX = range*2;
  int NY = NX;
  *nx = NX;
  *ny = NY;
  //*dist=range;  
  
  float cell_size = *grid_resol/1000.; 

  /*printf("%s \n",filename);
  printf("%d \n",INDEX);
  printf("%d %d \n",NX,NY);
  printf("%f \n",cell_size);*/

  ht = (1500.0 - radar->h.height)/1000.; //1500 -> m단위 고도
  INDEX = *var_name;

  cappi = RSL_cappi_at_h( radar->v[INDEX], ht, range);
  cappi->sweep = RSL_copy_sweep(radar->v[INDEX]->sweep[1]);
  //cappi->Er_loc
  carpi = RSL_cappi_to_carpi(cappi,cell_size,cell_size,cappi->lat,cappi->lon,NX,NY,NX/2,NY/2);

  //float elev = radar -> v[INDEX] -> sweep[2] -> h.elev;
	//printf("%7.2f \n",elev);

  //n=0;
  for(j=0;j<NY;j++)
    {
    for(i=0;i<NX;i++)
        {
           var[i][j] = carpi->f(carpi->data[j][i]); //좌측하단이 (1,1)
           //var[n]= carpi->f(carpi->data[NY-j-1][i]); //좌측상단이 (1,1)
			if (var[i][j] == 131072.0) var[i][j] =-99.9; //bad & noecho
           //n=n+1;
			//printf("%7.2f",var[j][i]);
        }
		//printf("\n");
  }
  

  RSL_free_carpi(carpi);
  RSL_free_cappi(cappi);
  RSL_free_radar(radar);

}

