/**************************************************************************************
*	Archivo: factorial.c
*	Autor: Washington Velasquez Vargas
*	Fecha de creacion: 05-11-08
*	
*	Descripcion:	Hacer un Programa que pida al usuario ingresar un numero, luego 
*					calcular el factorial de ese numero y presentar en pantalla dicho 
*					valor.(Utilizando ciclo "for")	
***************************************************************************************/

/*************************
*  I N C L U S I O N E S *
**************************/
#include <stdio.h>

/***********************
* F. P R I N C I P A L *
************************/
void main()
{
	int num,i,factor=1;
	printf("Ingrese un numero:");
	scanf("%d",&num);
	for(i=1;i<=num;i++)//Calcula el factorial del numero ingresado.
	{
		factor=factor*i;//va multiplicando factor por el incremento consecutivamente
	}
	printf("El factorial de %d es:%d",num,factor);
	getch();
}