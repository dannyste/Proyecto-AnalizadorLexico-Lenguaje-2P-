/********************************************************************************************
 *		Archivo:	 multiNum.c
 *		Autor:		 Washington Velasquez Vargas
 *		Fecha de Creación:		28-12-2008
 *		Descripción: Funcion para multiplicar dos numeros, mediante sumas sucesivas de manera recursiva.
 ********************************************************************************************/

/*************************
*  I N C L U S I O N E S *
**************************/
#include <stdio.h>

/***********************
* P R O T O T I P O S  *
************************/
int multiplicar_recursivo(int num1, int num2);

/***********************
* F. P R I N C I P A L *
************************/
void main()
{
	int num1,num2;
	printf("Ingrese primer numero Numero:\n");
	scanf("%d",&num1);
	printf("Ingrese el segundo numero Numero:\n");
	scanf("%d",&num2);
	printf("La multiplicacion es:%d\n",multiplicar_recursivo(num1,num2));
	getch();
		
}
/****************************************************************
 * Funcion: multiplicar_recursivo
 ****************************************************************
 * Modo de uso:
 * int multiplicar_recursivo(int num1, int num2);
 *	si se llama a la funcion con: multiplicar_recursivo(3,5); retornara 15
 ****************************************************************/
int multiplicar_recursivo(int num1, int num2)
{
	if(num2==0)
		return 0;
	
	if(num2>0)
	{
		num2--;
		return  (num1+multiplicar_recursivo(num1,num2));// Se van sumando los terminos hasta obtener el numero
	}			
}
