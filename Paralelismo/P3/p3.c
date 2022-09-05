//
// Created by sergio on 23/04/22.
//
#include <stdio.h>
#include <sys/time.h>
#include <mpi/mpi.h>

//COMPILAR -> mpicc -Wall p3.c -lm -o p3
//EXECUTAR -> mpirun -np NºPROCS ./p3

#define DEBUG 0

#define N 700

int main(int argc, char *argv[] ) {
    int i, j, filas_proc, filas_a_maiores, filas_finales;
    float matrix[N][N];
    float vector[N];
    float result[N];
    struct timeval  tv1, tv2;

    //Variables para procesos
    int numprocs, rank;

    //Iniciamos procesos
    MPI_Init(&argc , &argv);

    //A PARTIR DE AQUÍ CÓDIGO EN CADA PROCESO!!!
    MPI_Comm_size( MPI_COMM_WORLD, &numprocs);  //Obtemos o Nº de procesos que se crearon
    MPI_Comm_rank( MPI_COMM_WORLD, &rank);   //Obtemos identificador do proceso



    /* Initialize Matrix and Vector*/
    if(rank == 0) {  //Faino o proceso raíz, 0
        for (i = 0; i < N; i++) {
            vector[i] = i;
            for (j = 0; j < N; j++) {
                matrix[i][j] = i + j;
            }
        }
    }



    gettimeofday(&tv1, NULL);

    //1º Pasamos a todos os procesos o vector polo que se multiplica
    MPI_Bcast(vector, N, MPI_FLOAT, 0 , MPI_COMM_WORLD);

    gettimeofday(&tv2, NULL);
    int t_bcast = (tv2.tv_usec - tv1.tv_usec)+ 1000000 * (tv2.tv_sec - tv1.tv_sec);


    //Vemos se necesitamos rellenar a matriz con filas e se é así con cantas
    if(N % numprocs != 0){  //Se o reparto de filas non dá exacto
      //Vemos cantas filas pasamos a cada proceso
      filas_proc = (N/numprocs)+1;

      //Calculamos cantas filas necesitamos engadir
      filas_a_maiores = filas_proc*numprocs /*filas necesarias*/ - N /*filas originales*/;

      //Gardamos o novo Nº de filas da matriz
      filas_finales = N + filas_a_maiores;
    }else{
        //Se o reparto de filas dá exacto non engadimos filas
        filas_proc = N/numprocs;  //Filas a cada proceso
        filas_finales = N;   //Filas totales da matriz (NON CAMBIAN!)
    }

    //MATRIZ PARA FILAS DE CADA PROCESO garde nela os elementos das filas que lle corresponden
    float matriz_receptora[filas_proc][N];

    //Creamos a matriz coa que vamos traballar (con filas engadidas se é necesario!!!)
    float matrix_final[filas_finales][N];

    //Rellenamos a nova matriz total
    for(i = 0; i < N; i++){
        for (j = 0; j < N; j++) {
            matrix_final[i][j] = matrix[i][j];
        }
    }



    gettimeofday(&tv1, NULL);

    // Asignamos a cada proceso as súas filas correspondentes
    MPI_Scatter(matrix_final, filas_proc*N, MPI_FLOAT, matriz_receptora, filas_proc*N, MPI_FLOAT, 0, MPI_COMM_WORLD);

    gettimeofday(&tv2, NULL);
    int t_scatter = (tv2.tv_usec - tv1.tv_usec)+ 1000000 * (tv2.tv_sec - tv1.tv_sec);




    //Empezamos a contar tempo de computación
    gettimeofday(&tv1, NULL);


    //Creamos matriz para gardar os resultados das multiplicacións
    //Cada proceso multiplica as filas que lle tocaron polo vector!
    float result_proc[filas_proc];
    for(i=0;i<filas_proc;i++) {
        result_proc[i]=0;
        for(j=0;j<N;j++) {
            result_proc[i] += matriz_receptora[i][j]*vector[j];
        }
    }

    //Paramos de contar
    gettimeofday(&tv2, NULL);

    //TEMPO DE COMPUTACIÓN
    int microseconds_comp = (tv2.tv_usec - tv1.tv_usec)+ 1000000 * (tv2.tv_sec - tv1.tv_sec);




    gettimeofday(&tv1, NULL);

    //Recollemos RESULTADOS das multiplicacións de cada proceso nun único vector
    MPI_Gather(result_proc, filas_proc, MPI_FLOAT, result, filas_proc, MPI_FLOAT, 0, MPI_COMM_WORLD);

    gettimeofday(&tv2, NULL);
    int t_gather = (tv2.tv_usec - tv1.tv_usec)+ 1000000 * (tv2.tv_sec - tv1.tv_sec);

    //Calculamos o TEMPO TOTAL DE COMUNICACIÓN
    int t_total_comm = t_bcast + t_scatter + t_gather;


    //TEMPOS
    //Vectores para gardar tempos de computación e comunicación
    int microseconds_comp_procs[numprocs];
    int microseconds_comm_procs[numprocs];
    //Recollemos TEMPO DE COMPUTACIÓN de cada proceso nun único vector
    MPI_Gather(&microseconds_comp, 1, MPI_INT, microseconds_comp_procs, 1, MPI_INT, 0, MPI_COMM_WORLD);

    //Recollemos TEMPO DE COMUNICACIÓN de cada proceso nun único vector
    MPI_Gather(&t_total_comm, 1, MPI_INT, microseconds_comm_procs, 1, MPI_INT, 0, MPI_COMM_WORLD);



    /*Display result */
    if (DEBUG){
        if(rank == 0) {
            for (i = 0; i < N; i++) {
                printf(" %f \t ", result[i]);
            }
        }
    } else {
        if(rank == 0) {
            //Tempos por proceso
            for(i = 0; i < numprocs; i++) {
                printf("Proceso %d Time Computación  (seconds) = %lf\n", i, (double) microseconds_comp_procs[i] / 1E6);
                printf("Proceso %d Time Comunicación (seconds) = %lf\n\n", i, (double) microseconds_comm_procs[i] / 1E6);
            }
        }
    }


    MPI_Finalize();
    return 0;
}
