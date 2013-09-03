/**************************************************************************************
*	Archivo: triangulo.c
*	Autor: Washington Velasquez Vargas
*	Fecha de creacion: 18-11-08
*	
*	Descripcion:	Escriba el procedimiento triangulo, el cual dado un número entero n 
*					imprime un triángulo rectángulo de n líneas, de la siguiente forma:
*
*						Si n = 5					Si n = 7
*						triangulo (5);			triangulo (7);
*						1						1
*						2 3 2					2 3 2
*						3 4 5 4 3				3 4 5 4 3
*						4 5 6 7 6 5 4			4 5 6 7 6 5 4
*						5 6 7 8 9 8 7 6 5		5 6 7 8 9 8 7 6 5
*												6 7 8 9 10 11 10 9 8 7 6
*												7 8 9 10 11 12 13 12 11 10 9 8 7
*
***************************************************************************************/

/*************************
*  I N C L U S I O N E S *
**************************/
#include <stdio.h>

/*********************
*P R O T O T I P O S
*********************/
/*
*	Funcion:  void triangulo(int numero); 
*	Descripcion:	Procedimiento que tiene como parametros un entero
*					e imprime lo dicho previamente.
*/
void triangulo(int numero); 

/***********************
* F. P R I N C I P A L *
************************/

void main()
{
	int num;
	printf("Ingrese un numero:");
	scanf("%d",&num);
	triangulo(num);
	getch();
}


/********************************
* I M P L E M E N T A C I O N E S
*********************************/

void triangulo(int numero)
{
	int i,j,tmp;
	printf("1\n");
	for(i=2;i<=numero;i++)
	{
		tmp=i;
		for(j=i;j<=(tmp+(tmp-1));j++)
		{
			printf("%d ",j);
		}
		for(j=(tmp+(tmp-2));j>=i;j--)
		{
			printf("%d ",j);
		}
		printf("\n");
	}
}