//examen parcial 2003T1p(1)
#include<stdio.h>
void triangulo(int lineas, char caracter);
void main()
{
	triangulo(3,'a');
	getch();
}
void triangulo(int lineas, char caracter)
{
	int i,j;
	for(j=0;j<lineas;j++)
	{
		for(i=0;i<j;i++)
			printf(" ");
		for(i=lineas;i>j;i--)
			printf("%c",caracter);
		printf("\n");
	}
	return;
}