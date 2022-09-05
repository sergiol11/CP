#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi/mpi.h>


//COMPILAR -> mpicc -Wall p1.c -lm -o p1
//EXECUTAR -> mpirun -np  NºPROCS ./p1


int main(int argc, char *argv[]){
    int i, n, count = 0, count_proceso;
    double PI25DT = 3.141592653589793238462643;
    double pi, x, y, z;

    //Variables
    int numprocs, numprocs_copia, rank;

    //Parte inicial
    MPI_Init(&argc , &argv);   //Iniciamos procesos

    //A PARTIR DE AQUÍ CÓDIGO EN CADA PROCESO!!!
    MPI_Comm_size( MPI_COMM_WORLD, &numprocs);  //Obtemos o Nº de procesos que se crearon
    MPI_Comm_rank( MPI_COMM_WORLD, &rank);   //Obtemos identificador do proceso

    numprocs_copia = numprocs;  //Dámoslle valor á copia

    while(1) {
        if (rank == 0) {  //Se somos o proceso 0 (proceso principal)
            printf("Enter the number of points: (0 quits) \n");
            scanf("%d", &n);  //Gardamos en "n" os intentos que vamos facer

            for (int j = 1; j < numprocs; j++) {  //Avisamos a todos os procesos (menos a min mesmo, por eso i = 1) de canto vale n
                MPI_Send(&n, 1, MPI_INT, j, 0, MPI_COMM_WORLD);
            }
        } else {  //Para o resto de procesos creados gardamos en "n" o seu valor (enviado polo proceso 0)
            MPI_Recv(&n, 1, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUSES_IGNORE);
        }

        //Se "n" vale 0 acabamos execución
        if(n == 0){
            break;
        }


        //Restablecemos contadores (count_procesos non fai falta porque en cada mensaje sobreescríbese o valor!)
        count = 0;


        //Bucle que realizan todos os procesos
        for (i = rank + 1; i <= n; i += numprocs) {    //Reparto cíclico
            // Get the random numbers between 0 and 1
            x = ((double) rand()) / ((double) RAND_MAX);
            y = ((double) rand()) / ((double) RAND_MAX);

            // Calculate the square root of the squares
            z = sqrt((x * x) + (y * y));

            // Check whether z is within the circle
            if (z <= 1.0) {
                count++;
            }
        }


        if (rank == 0) {  //Se somos o proceso 0 (proceso principal)
            while (numprocs_copia > 1) {  //Esperamos a que cada un dos outros procesos (por eso o 1!!) nos digan cantos puntos rexistraron dentro do círculo
                MPI_Recv(&count_proceso, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUSES_IGNORE);

                //Decrementamos numprocs porque xa recibimos os puntos que entraron dun proceso máis
                numprocs_copia--;

                //Vamos sumando para obter o número de puntos que entraron no círculo sumando o de todos os procesos
                count += count_proceso;
            }

            //Estimamos PI e escribímolo
            pi = ((double) count / (double) n) * 4.0;
            printf("pi is approx. %.16f, Error is %.16f\n", pi, fabs(pi - PI25DT));

            //Restablecemoso o valor da copia do Nº de procesos para seguinte iteración
            numprocs_copia = numprocs;
        } else {  //Para o resto de procesos, enviamos ao proceso 0 (principal) o número de puntos probados que entraron no círculo
            MPI_Send(&count, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);
        }
    }

    MPI_Finalize();
}