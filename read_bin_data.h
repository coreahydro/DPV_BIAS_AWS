#ifndef SITE_R_H_
#define SITE_R_H_

typedef struct
{
    char file_type[20];
    char site_name[8]; 
    int xdim; 
    int ydim; 
    int resol;
    int range; 
//    int sweep_num;  
//    float height;
    float lat;
    float lon; 
}SITE_HEADER;

typedef struct
{   char CZ[2];
}SITE_CZ;

typedef struct
{
    char DR[2];
}SITE_DR;

typedef struct
{
    char KD[2];
}SITE_KD;

typedef struct
{
    char RH[2];
}SITE_RH;

extern SITE_CZ h_CZ;
extern SITE_DR h_DR;
extern SITE_KD h_KD;
extern SITE_RH h_RH;

#endif 
