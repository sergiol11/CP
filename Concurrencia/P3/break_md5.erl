-module(break_md5).
-define(PASS_LEN, 6).
-define(UPDATE_BAR_GAP, 1000).
-define(BAR_SIZE, 40).
-define(NUM_PROCS, 12).

-export([break_md5/1,
  pass_to_num/1,
  num_to_pass/1,
  num_to_hex_string/1,
  hex_string_to_num/1,
  break_md5s/1
  , pids_loop/1, counter_loop/3, break_beta/9]).

-export([progress_loop/8]).

% Base ^ Exp

pow_aux(_Base, Pow, 0) ->
    Pow;
pow_aux(Base, Pow, Exp) when Exp rem 2 == 0 ->
    pow_aux(Base*Base, Pow, Exp div 2);
pow_aux(Base, Pow, Exp) ->
    pow_aux(Base, Base * Pow, Exp - 1).

pow(Base, Exp) -> pow_aux(Base, 1, Exp).

%% Number to password and back conversion

num_to_pass_aux(_N, 0, Pass) -> Pass;
num_to_pass_aux(N, Digit, Pass) ->
    num_to_pass_aux(N div 26, Digit - 1, [$a + N rem 26 | Pass]).

num_to_pass(N) -> num_to_pass_aux(N, ?PASS_LEN, []).

pass_to_num(Pass) ->
    lists:foldl(fun (C, Num) -> Num * 26 + C - $a end, 0, Pass).

%% Hex string to Number

hex_char_to_int(N) ->
    if (N >= $0) and (N =< $9) -> N - $0;
       (N >= $a) and (N =< $f) -> N - $a + 10;
       (N >= $A) and (N =< $F) -> N - $A + 10;
       true                    -> throw({not_hex, [N]})
    end.

int_to_hex_char(N) ->
    if (N >= 0)  and (N < 10) -> $0 + N;
       (N >= 10) and (N < 16) -> $A + (N - 10);
       true                   -> throw({out_of_range, N})
    end.

hex_string_to_num(Hex_Str) ->
    lists:foldl(fun(Hex, Num) -> Num*16 + hex_char_to_int(Hex) end, 0, Hex_Str).

num_to_hex_string_aux(0, Str) -> Str;
num_to_hex_string_aux(N, Str) ->
    num_to_hex_string_aux(N div 16,
                          [int_to_hex_char(N rem 16) | Str]).

num_to_hex_string(0) -> "0";
num_to_hex_string(N) -> num_to_hex_string_aux(N, []).


%% Progress bar runs in its own process
%Se xa non quedan hashes por averiguar
progress_loop(_, _, _, _, _, _, [], Dad_Pid) -> Dad_Pid ! barra_finished;   %Avisamos ao proceso principal de que acabou a barra;

progress_loop(N, Bound, Mirados, Tempo_Anterior, Iteracions, Velocidad, Target_Hashes, Dad_Pid) ->
    receive
        limite_superado -> io:format("~nPassword out of range!~n"),
                          Dad_Pid ! barra_finished;   %Avisamos ao proceso principal de que acabou a barra

      {encontramos, Hash, Pass} ->
        io:format("~n\e[2K\r~.16B: ~s~n", [Hash, Pass]),   %escribimos a contraseña encontrada
        progress_loop(N, Bound, Mirados, Tempo_Anterior, Iteracions, Velocidad, lists:delete(Hash, Target_Hashes), Dad_Pid);  %e quitámos o hash da lista de hashes a buscar


        {progress_report, Checked} ->   %Avísannos de que actualicemos a barra de progreso e debaixo compoñemos a barra
            N2 = N + Checked,
            Full_N = N2 * ?BAR_SIZE div Bound,
            Full = lists:duplicate(Full_N, $=),
            Empty = lists:duplicate(?BAR_SIZE - Full_N, $-),

            %Cálculo da velocidad
          if Iteracions == 3 ->    %Recalcular
            T2 = erlang:monotonic_time(microsecond),
            Diferencia = T2 - Tempo_Anterior,
            Pswd_Leidos_En_X_Iteracions = Mirados + Checked,  %Os que había comprobado de antes + "checked" os que nos dixeron que se comprobaron ahora
            Velocidad_Nova = Pswd_Leidos_En_X_Iteracions / (Diferencia / 1000000),

            io:format("\r[~s~s] ~.2f%  ~.2f pswd/seg", [Full, Empty, N2/Bound*100, Velocidad_Nova]),
            progress_loop(N2, Bound, 0, T2, 0, Velocidad_Nova, Target_Hashes, Dad_Pid); %Volvemos a 0 os mirados para volver a acumular nas 3 iteracións

          true ->    %Seguimos sin actualizar a velocidad
           io:format("\r[~s~s] ~.2f%  ~.2f pswd/seg", [Full, Empty, N2/Bound*100, Velocidad]),
           progress_loop(N2, Bound, Mirados + Checked, Tempo_Anterior, Iteracions + 1, Velocidad, Target_Hashes, Dad_Pid)
        end
    end.




%  Xa non quedan hashes por romper, paramos os breakers coa lista vacía de hashes
break_beta([], _, _, _, _, _, _, _, Dad_Pid) ->
  Dad_Pid ! {para, []};   %Avisamos proceso principal de que paramos


%  Xa comprobamos todas as contraseñas e quedan hashes por encontrar entón paramos os procesos que fan break
break_beta(Target_Hashes, Bound, _, _, _, Bound, _, Bound, Dad_Pid) ->
  Dad_Pid ! {para, Target_Hashes};  %Avisamos proceso principal de que paramos


%  Chegamos ao final do rango de proba
break_beta(Target_Hashes, _, Progress_Pid, Counter_Pid, Pids_Backup, Fin, Fin, Bound, Dad_Pid) ->
  Counter_Pid ! {informame, self()},    % Solicitamos que nos digan cantos hashes se levan probados entre todos!!!
  receive
    {toma_info, New_Num_Hashes_Probados} ->
      New_Ini = New_Num_Hashes_Probados,
      % Actualizamos o rango de proba do proceso
      break_beta(Target_Hashes, New_Num_Hashes_Probados, Progress_Pid, Counter_Pid, Pids_Backup, New_Ini, New_Ini + ?UPDATE_BAR_GAP, Bound, Dad_Pid)
  end;

% BREAK PRINCIPAL
break_beta(Target_Hashes, Num_Hashes_Probados, Progress_Pid, Counter_Pid, Pids_Backup, Ini, Fin, Bound, Dad_Pid) ->
  receive
    {borra_hash, Hash} ->  %  Recibimos aviso de que temos que borrar hash concreto
      New_Target_Hashes = lists:delete(Hash, Target_Hashes),  %Borramos ese hash
      break_beta(New_Target_Hashes, Num_Hashes_Probados, Progress_Pid, Counter_Pid, Pids_Backup, Ini, Fin, Bound, Dad_Pid)

  after 0 ->

  %Creamos un hash co que probar
  Pass = num_to_pass(Ini),
  Hash = crypto:hash(md5, Pass),
  Num_Hash = binary:decode_unsigned(Hash),

  %Gardamos en variable se ese hash coincide ou non con algún dos que hai que romper
  Buscado = lists:member(Num_Hash, Target_Hashes),

  if
    Buscado == true ->  %Se hash a buscar coincide cun da lista
           Progress_Pid ! {encontramos, Num_Hash, Pass},  %Avisamos ao proceso da barra para que o borre
           Pids_Backup ! {dame_pids, self()},  %Pedimos pids de breakers
           receive
           {aqui_tes, Pids_Breakers} -> notify_breakers(Pids_Breakers, {borra_hash, Num_Hash})   %Avisamos a todos os procesos que fan break para que o borren
           end;
    true -> ok
  end,
  break_beta(Target_Hashes, Num_Hashes_Probados, Progress_Pid, Counter_Pid, Pids_Backup, Ini + 1, Fin, Bound, Dad_Pid)
end.






%  Avisar a todos os procesos que rompen hashes de algo
notify_breakers([], _) -> acaba_notify_breakers;

notify_breakers(Pids, Notification) ->
  hd(Pids) ! Notification,
  notify_breakers(tl(Pids), Notification)
.


%  Acumulador/Informador de número de hashes probados
counter_loop(Num_Hashes_Probados, Progress_Pid, Bound) ->
  receive
    {informame, Pid_Process} -> Pid_Process ! {toma_info, Num_Hashes_Probados}, %Informamos ao proceso que nolo pide dos hashes probados ata o momento

                                if Num_Hashes_Probados < Bound ->  %Se o número de hashes probado actual non iguala ou supera  100%
                                  Progress_Pid ! {progress_report, ?UPDATE_BAR_GAP};   %Notificamos mandando mensaje ao PID da barra de progreso para que aumente

                                  true -> ok  %Se o número de hashes actuales iguala ou supera o 100% non actualizamos a barra de progreso
                                end,

                                Limite = min(Bound, Num_Hashes_Probados + ?UPDATE_BAR_GAP),   %Garantizamos que nn nos pasamos de Bound (así nunca informamos de + de Bound)
                                counter_loop(Limite, Progress_Pid, Bound);

    % Paramos este proceso
    stop -> counter_para
  end
.


%  Acumulador/Informador de Pids de procesos que fan break
pids_loop(Pids) ->
  receive
   {garda, Pid_Save} -> pids_loop([Pid_Save | Pids]);

   {dame_pids, Pid_Process} -> Pid_Process ! {aqui_tes, Pids},
                               pids_loop(Pids);

   stop -> pid_backup_para
  end
.



% Xa creamos os procesos que fan break indicados (volvemos a break_md5s)
create_processes(0, _, _, _, _, _, _, _, _) -> ok;

%Creamos un número de procesos determinado que fan break
create_processes(Num_processes, Ini, Fin, Diferencia, Progress_Pid, Counter_Pid, Pids_Backup, Target_Hashes, Bound) ->
  if Num_processes > 0 ->
    Pid = spawn(?MODULE, break_beta, [Target_Hashes,  ?NUM_PROCS * Diferencia, Progress_Pid, Counter_Pid, Pids_Backup, Ini, Fin, Bound, self()]),
    Pids_Backup ! {garda, Pid},  %Vamos gardando PIDs dos procesos que fan break
    create_processes(Num_processes - 1, Fin, Fin + Diferencia, Diferencia, Progress_Pid, Counter_Pid, Pids_Backup, Target_Hashes, Bound)
  end
.


% Se acabaron todos os procesos que fan break e non quedan hashes por encontrar volvemos a break_md5s (a barra de progreso xa parou ao ver que non quedan hashes)
comprobar_salida(0, [], _) -> ok;

% Se acabaron todos os procesos que fan break e quedan hashes por buscar mandamos aviso á barra de progreso desta situación (imprime mensaje de aviso e para)
comprobar_salida(0, _, Progress_Pid) -> Progress_Pid ! limite_superado;

% Recibimos un mensaje cada vez que para un proceso que fai break e vamos diminuindo o nº de procesos ata chegar a 0
comprobar_salida(Numero_Procesos, _, Progress_Pid) ->
  receive
    {para, New_Target_Hashes} -> comprobar_salida(Numero_Procesos - 1, New_Target_Hashes, Progress_Pid)
  end
.


%% Break a hash
break_md5(Hash) -> break_md5s([Hash]).

%Break a list of hashes
break_md5s(Hashes) ->
  Bound = pow(26, ?PASS_LEN),   %Nº de contraseñas
  T1 = erlang:monotonic_time(microsecond),  %Empezamos a contar para mostrar velocidad na barra
  Target_Hashes = lists:map(fun hex_string_to_num/1, Hashes),     %Traducimos de hexadecimal a Nº todos os hashes da lista

  %  Creo un novo proceso para que mostre a barra
  Progress_Pid = spawn(?MODULE, progress_loop, [0, Bound, 0, T1, 0, 0.00, Target_Hashes, self()]),

  %  Creo un proceso que vaia gardando o número de hashes probados
  Counter_Pid = spawn(?MODULE, counter_loop, [?NUM_PROCS * ?UPDATE_BAR_GAP, Progress_Pid, Bound]),

  %  Creo proceso para gardar os PID dos procesos que rompen hashes (fan break)
  Pids_Backup = spawn(?MODULE, pids_loop, [[]]),

  %Función que crea os procesos que fan break
  create_processes(?NUM_PROCS, 0, ?UPDATE_BAR_GAP, ?UPDATE_BAR_GAP, Progress_Pid, Counter_Pid, Pids_Backup, Target_Hashes, Bound),

  %Esperamos a que os procesos que fan break acaben
  comprobar_salida(?NUM_PROCS, Target_Hashes, Progress_Pid),

  %Esperamos a que a barra nos diga que rematou
  receive
    barra_finished -> ok
  end,

  Counter_Pid ! stop,
  Pids_Backup ! stop,
  ok.