#include <stdio.h>
#include <stdlib.h>
#include <zlib.h>
#include "read_bin_data.h"

SITE_HEADER h_r;

SITE_CZ h_CZ;
SITE_DR h_DR;
SITE_KD h_KD;
SITE_RH h_RH;

float* read_bin_(char *filename,int *nrow, int *ncol, int *R,float data[*ncol][*nrow],int *ny,int *nx)
//float data[*ncol][*nrow];
{
    gzFile fp = NULL;
    int skip = 0;
    float *imsi_data;
    int data_size,i,x,y,n;

    for(i=0;i<256;i++){ if(filename[i]==' '){filename[i]='\0';break;} }

    //printf("%s \n",filename);
    //printf("%d \n",*R);
 
    if( (fp = gzopen(filename, "r")) == NULL ) {
        return NULL;
    }
    gzread(fp,&h_r, sizeof(SITE_HEADER));

    data_size = sizeof(float)*h_r.xdim*h_r.ydim;
    *ny=h_r.ydim;
    *nx=h_r.xdim;
    //*sitelon = h_r.lon;
    //*sitelat = h_r.lat;
    /*printf("%s \n",h_r.file_type);
    printf("%s \n",h_r.site_name);    
    printf("%d %d \n",h_r.xdim,h_r.ydim);
    printf("%f %f \n",h_r.lon,h_r.lat);    
    printf("%d \n",h_r.range);*/
    
    imsi_data = (float*)malloc(data_size);

    skip += data_size*(*R-1);

    switch(*R)
    {
    case 5:
        skip += sizeof(SITE_RH);
    case 4:
        skip += sizeof(SITE_KD);
    case 3:
        skip += sizeof(SITE_DR);
    case 2:
        skip += sizeof(SITE_CZ);
            break;
    default:
        skip = 0;
        break;         
    }

    gzseek(fp, skip, SEEK_CUR);
		n=0;
    switch(*R)
    { 
        case 1:
            gzread(fp, &h_CZ, sizeof(SITE_CZ));
            gzread(fp, imsi_data,data_size);
            for(y=0;y<h_r.ydim;y++) //ydim
            {
            	for(x=0;x<h_r.xdim;x++) //xdim
            	{
            		data[x][y]=imsi_data[n];
            		//data[x][h_r.ydim-1-y]=imsi_data[n];
            		n=n+1;
            		//printf("%9.2f",data[x][y]);
          		}
          		//printf("\n");
        		}
        
            break;
        case 2:
            gzread(fp, &h_DR, sizeof(SITE_DR));
            gzread(fp, imsi_data,data_size);
            for(y=0;y<h_r.ydim;y++) //ydim
            {
            	for(x=0;x<h_r.xdim;x++) //xdim
            	{
            		data[x][y]=imsi_data[n];
            		//data[x][h_r.ydim-1-y]=imsi_data[n];
            		n=n+1;
            		//printf("%9.2f",data[x][y]);
          		}
          		//printf("\n");
        		}

            break; 
        case 3:
            gzread(fp, &h_KD, sizeof(SITE_KD));
            gzread(fp, imsi_data,data_size);
            for(y=0;y<h_r.ydim;y++) //ydim
            {
            	for(x=0;x<h_r.xdim;x++) //xdim
            	{
            		data[x][y]=imsi_data[n];
            		//data[x][h_r.ydim-1-y]=imsi_data[n];
            		n=n+1;
          		}
        		}

            break; 
        case 4:
            gzread(fp, &h_RH, sizeof(SITE_RH));
            gzread(fp, imsi_data,data_size);
            for(y=0;y<h_r.ydim;y++) //ydim
            {
            	for(x=0;x<h_r.xdim;x++) //xdim
            	{
            		data[x][y]=imsi_data[n];
            		//data[x][h_r.ydim-1-y]=imsi_data[n];
            		n=n+1;
            		//printf("%12.4f",data[x][y]);
          		}
          		//printf("\n");
        		}

            break; 
    }
    
    gzclose(fp);
    return imsi_data;
}

