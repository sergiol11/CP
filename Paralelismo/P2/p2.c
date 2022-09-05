//
// Created by sergio on 3/4/22.
//
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi/mpi.h>


//COMPILAR -> mpicc -Wall p2.c -lm -o p2
//EXECUTAR -> mpirun -np NºPROCS ./p2


int MPI_FlattreeColectiva(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm){
    int rank, numprocs, i, error;

    //Como parámetros teñen que ser os mismos e non sabemos Nº de procesos temos que recalcular aquí
    //Como ademáis tampouco sabemos que proceso somos nós mismos, "rank" entón recalculamos tamén de novo
    MPI_Comm_size(comm, &numprocs);  //Obtemos o Nº de procesos que se crearon
    MPI_Comm_rank(comm, &rank);   //Obtemos identificador do proceso

    //Primeiro controlamos errores
    if(comm == NULL){
        return MPI_ERR_COMM;
    }
    if(count < 0){
        return MPI_ERR_COUNT;
    }
    if(datatype != MPI_INT){
        return MPI_ERR_TYPE;
    }
    if(sendbuf == NULL || recvbuf == NULL || sendbuf == recvbuf){
        return MPI_ERR_BUFFER;
    }
    if(root < 0 || root >= numprocs){
        return MPI_ERR_ROOT;
    }
    if(op != MPI_SUM){
        return MPI_ERR_OP;
    }


    //Facemos cast do buffer no que vamos gardar resultado final
    int * recvbuff_aux = (int *)recvbuf;
    //Creamos buffer para as recepcións (así non machamos sendbuf seguido, solo se poden cambiar variables creadas dentro desta función, as que recibimos como parámetro NO!!)
    int buff_recepcion[count];


    if (rank == root) {  //Se somos o proceso raíz
        //Cast para o buffer do propio proceso raíz
        int * buff_aux = (int *)sendbuf;

        //Inicializamos os valores do buffer de resposta cos do root
        for(i = 0; i < count; i++){
            recvbuff_aux[i] = buff_aux[i];
        }

        while (numprocs > 1) {  //Esperamos a que cada un dos outros procesos nos manden información
            error = MPI_Recv(buff_recepcion, count, datatype, MPI_ANY_SOURCE, MPI_ANY_TAG, comm, MPI_STATUSES_IGNORE);
            if(error != MPI_SUCCESS){  //Controlamos se Recv nos devolve error
                return error;
            }

            //Decrementamos numprocs porque xa recibimos aproximación dun proceso máis
            numprocs--;

            //Recreamos o que faría MPI_Reduce() (en cada elemento do buffer resultado sumar os valores dos elementos correspondentes dos buffers que se reciben)
            for(i = 0; i < count; i++){
                recvbuff_aux[i] += buff_recepcion[i];
            }
        }
    } else {  //Para o resto de procesos, enviamos o buffer (co número de puntos que entraron) ao proceso root
        error = MPI_Send(sendbuf, count, datatype, root, 0, comm);
        if(error != MPI_SUCCESS){  //Controlamos se Recv nos devolve error
            return error;
        }
    }

    return MPI_SUCCESS;  //Retornamos que a función tivo éxito
}



int MPI_BinomialBcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm){
    int rank, numprocs, i, error;

    MPI_Comm_size(comm, &numprocs);  //Obtemos o Nº de procesos que hai no comunicador
    MPI_Comm_rank(comm, &rank);   //Obtemos identificador do proceso dentro do comunicador

    //Controlamos errores
    if(comm == NULL){
        return MPI_ERR_COMM;
    }
    if(count < 0){
        return MPI_ERR_COUNT;
    }
    if(datatype != MPI_INT){
        return MPI_ERR_TYPE;
    }
    if(buffer == NULL){
        return MPI_ERR_BUFFER;
    }
    if(root < 0 || root >= numprocs){
        return MPI_ERR_ROOT;
    }



    //Se non somos o proceso 0, root, esperamos aquí a recibir n (O PROCESO ROOT É O QUE INICIA todoo ABAIXO!)
    //Unha vez recibamos xa podemos enviar no bucle de abaixo
    if(rank != root){
        error = MPI_Recv(buffer, count, datatype, MPI_ANY_SOURCE, MPI_ANY_TAG, comm, MPI_STATUSES_IGNORE);
        if(error != MPI_SUCCESS){
            return error;
        }
    }


    //Ao ir o bucle multiplicándose por 2 xa vai tendo o valor da condición (e claro empeza en 1, 2^0)
    for(i = 1 ;; i *= 2){   //É decir, "i" sempre vale o correspondiente a 2^(i-1)
        if(rank < i){  //Comprobamos se o proceso cumple a condición para mandar "n" a outro
            if(rank + i >= numprocs){  //Ademáis da condición de arriba vemos se ao proceso lle toca mandar "n" a un que xa non existe
                break;
            }

            //Se non nos pasamos enviamos mensaje ao proceso correspondiente
            error = MPI_Send(buffer, count, datatype, rank + i, 0, comm);
            if(error != MPI_SUCCESS){
                return error;
            }
        }
    }

    return MPI_SUCCESS;
}

int main(int argc, char *argv[]){
    int i, n, count = 0, count_total;
    double PI25DT = 3.141592653589793238462643;
    double pi, x, y, z;

    //Variables
    int numprocs, rank;

    //Parte inicial
    MPI_Init(&argc , &argv);   //Iniciamos procesos

    //A PARTIR DE AQUÍ CÓDIGO EN CADA PROCESO!!!
    MPI_Comm_size( MPI_COMM_WORLD, &numprocs);  //Obtemos o Nº de procesos que se crearon
    MPI_Comm_rank( MPI_COMM_WORLD, &rank);   //Obtemos identificador do proceso


    while(1) {
        if (rank == 0) {  //Se somos o proceso 0 (proceso principal)
            printf("Enter the number of points: (0 quits) \n");
            scanf("%d", &n);  //Gardamos en "n" os intentos que vamos facer
        }

        //Desde o proceso raiz (0) enviamos valor de "n" a todos
        MPI_BinomialBcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);

        //Se "n" vale 0 acabamos execución
        if(n == 0){
            break;
        }

        //Restablecemos contadores
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

        //Recollemos no proceso raíz (0) a suma de todos os contadores "en count_total"
        MPI_FlattreeColectiva(&count, &count_total, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);

        if (rank == 0) {  //Se somos o proceso 0 (proceso principal)
            //Estimamos PI e escribímolo
            pi = ((double) count_total / (double) n) * 4.0;
            printf("pi is approx. %.16f, Error is %.16f\n", pi, fabs(pi - PI25DT));
        }
    }

    MPI_Finalize();
}
