       IDENTIFICATION DIVISION.
       PROGRAM-ID. Sistema-Bancario.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL AccountFile ASSIGN TO "cuentas.dat"
               ORGANIZATION IS SEQUENTIAL.
           SELECT OPTIONAL TransactionFile ASSIGN TO "transacciones.dat"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  AccountFile.
       01  AccountRecord.
           05  Account-ID           PIC 9(5).       * ID de la cuenta (número de cuenta).
           05  Account-Holder       PIC X(30).      * Nombre del titular de la cuenta.
           05  Account-Balance      PIC 9(7)V99.    * Saldo de la cuenta.

       FD  TransactionFile.
       01  TransactionRecord.
           05  Trans-Account-ID     PIC 9(5).       * ID de la cuenta de la transacción.
           05  Trans-Type           PIC X(1).       * Tipo de transacción (D para depósito, W para retiro).
               88 Deposit            VALUE 'D'.      * Indicador para depósitos.
               88 Withdraw           VALUE 'W'.      * Indicador para retiros.
           05  Trans-Amount         PIC 9(7)V99.    * Monto de la transacción.

       WORKING-STORAGE SECTION.
       01  Prompt-Account-ID  PIC X(25)
       VALUE "Ingrese ID de la cuenta:".
       01  Prompt-Holder-Name PIC X(28)
       VALUE "Ingrese nombre del titular: ".
       01  Prompt-Amount      PIC X(25)
       VALUE "Ingrese monto: ".
       01  Invalid-Amount     PIC X(35)
       VALUE "Monto no válido, intente de nuevo.".
       01  Insufficient-Funds PIC X(51)
       VALUE "Fondos insuficientes para realizar la transacción.".

       01  User-Option        PIC X.                 * Opción elegida por el usuario en el menú.
       01  Found-Account      PIC X VALUE 'N'.       * Indicador de si se encontró la cuenta.
       01  Account-Search-ID  PIC 9(5).              * ID de la cuenta que el usuario está buscando.
       01  Transaction-Amount PIC 9(7)V99.           * Monto de la transacción.

       PROCEDURE DIVISION.
       Main-Logic.
           PERFORM Display-Menu                           * Muestra el menú principal.
           PERFORM UNTIL User-Option = '5'               * Repite hasta que el usuario elija salir.
               PERFORM Process-Option                     * Procesa la opción elegida.
               PERFORM Display-Menu                       * Muestra el menú nuevamente.
           END-PERFORM.
           STOP RUN.

       Display-Menu.
           DISPLAY "====== SISTEMA BANCARIO ======"
           DISPLAY "1. Crear cuenta"
           DISPLAY "2. Depositar dinero"
           DISPLAY "3. Retirar dinero"
           DISPLAY "4. Consultar saldo"
           DISPLAY "5. Salir"
           DISPLAY "Seleccione una opción: "
           ACCEPT User-Option.

       Process-Option.
           EVALUATE User-Option                          * Evalúa la opción seleccionada.
               WHEN '1'
                   PERFORM Create-Account               * Llama a la función para crear una cuenta.
               WHEN '2'
                   PERFORM Deposit-Money                * Llama a la función para depositar dinero.
               WHEN '3'
                   PERFORM Withdraw-Money               * Llama a la función para retirar dinero.
               WHEN '4'
                   PERFORM Check-Balance                 * Llama a la función para consultar saldo.
               WHEN OTHER
                   DISPLAY "Opción no válida, intente de nuevo."  * Mensaje para opción no válida.
           END-EVALUATE.

       Create-Account.
           OPEN OUTPUT AccountFile                       * Abre el archivo de cuentas para escritura.
           DISPLAY Prompt-Account-ID                    * Solicita el ID de la cuenta.
           ACCEPT Account-ID                              * Acepta el ID de la cuenta.
           DISPLAY Prompt-Holder-Name                   * Solicita el nombre del titular.
           ACCEPT Account-Holder                         * Acepta el nombre del titular.
           MOVE 0 TO Account-Balance                     * Inicializa el saldo de la cuenta en 0.
           WRITE AccountRecord                           * Escribe el registro de la nueva cuenta en el archivo.
           DISPLAY "Cuenta creada exitosamente."         * Mensaje de éxito.
           CLOSE AccountFile.                            * Cierra el archivo de cuentas.

       
       Deposit-Money.
           OPEN I-O AccountFile                          * Abre el archivo de cuentas para escritura.
           PERFORM Find-Account                          * Busca la cuenta del usuario.
           IF Found-Account = 'Y'                        * Verifica si la cuenta fue encontrada.
           DISPLAY Prompt-Amount                         * Solicita el monto a depositar.
           ACCEPT Transaction-Amount                    * Acepta el monto de la transacción.
           IF Transaction-Amount > 0                     * Verifica si el monto es mayor que 0.
                ADD Transaction-Amount TO Account-Balance     * Suma el monto al saldo de la cuenta.
                REWRITE AccountRecord                    * Actualiza el registro de la cuenta en el archivo.
                MOVE 'D' TO Trans-Type                    * Establece el tipo de transacción como depósito.
                PERFORM Record-Transaction                * Registra la transacción.
                DISPLAY "Depósito exitoso."                * Mensaje de éxito.
           ELSE
                DISPLAY Invalid-Amount                    * Mensaje para monto no válido.
           END-IF
           ELSE
               DISPLAY "Cuenta no encontrada."              * Mensaje si la cuenta no fue encontrada.
           END-IF.
           CLOSE AccountFile.

       Withdraw-Money.
           OPEN I-O AccountFile          
               PERFORM Find-Account                         * Busca la cuenta del usuario.
           IF Found-Account = 'Y'                           * Verifica si la cuenta fue encontrada.
               DISPLAY Prompt-Amount                        * Solicita el monto a retirar.
               ACCEPT Transaction-Amount                     * Acepta el monto de la transacción.
               IF Transaction-Amount > 0 AND 
                  Transaction-Amount <= Account-Balance       * Verifica si el monto es válido y no excede el saldo.
                   SUBTRACT Transaction-Amount FROM Account-Balance  * Resta el monto del saldo de la cuenta.
                   MOVE 'W' TO Trans-Type                     * Establece el tipo de transacción como retiro.
                   PERFORM Record-Transaction                 * Registra la transacción.
                   DISPLAY "Retiro exitoso."                  * Mensaje de éxito.
               ELSE IF Transaction-Amount > Account-Balance
                   DISPLAY Insufficient-Funds                 * Mensaje si no hay suficientes fondos.
               ELSE
                   DISPLAY Invalid-Amount                     * Mensaje para monto no válido.
               END-IF
           ELSE
               DISPLAY "Cuenta no encontrada."                * Mensaje si la cuenta no fue encontrada.
           END-IF.

           CLOSE AccountFile. 
       Check-Balance.
           OPEN I-O AccountFile 
           PERFORM Find-Account                            * Busca la cuenta del usuario.
           IF Found-Account = 'Y'                            * Verifica si la cuenta fue encontrada.
               DISPLAY "Saldo actual de la cuenta: ", Account-Balance   * Muestra el saldo actual.
           ELSE
               DISPLAY "Cuenta no encontrada."                * Mensaje si la cuenta no fue encontrada.
           END-IF.
           CLOSE AccountFile.
       Find-Account.
           MOVE 'N' TO Found-Account                     * Inicializar indicador de cuenta encontrada.
           DISPLAY Prompt-Account-ID                     * Solicitar ID de la cuenta a buscar.
           ACCEPT Account-Search-ID                       * Aceptar el ID de la cuenta a buscar.
           PERFORM UNTIL Found-Account = 'Y'             * Bucle hasta que se encuentre la cuenta.
           READ AccountFile                             * Leer el registro de la cuenta.
            AT END                                        *Buscar hasta el final del archivo .dat
                DISPLAY "Cuenta no encontrada."            * Mensaje si no se encuentra la cuenta.
                EXIT PERFORM
            NOT AT END                                     *Si antes de finalizar la busqueda encuentra la entrada procede
                IF Account-ID = Account-Search-ID            * Verificar si el ID coincide.
                    MOVE 'Y' TO Found-Account                * Marcar la cuenta como encontrada.
                END-IF
               END-READ
           END-PERFORM.
       Record-Transaction.
           OPEN EXTEND TransactionFile                  * Abre el archivo de transacciones para agregar nuevos registros.
           MOVE Account-ID TO Trans-Account-ID          * Asigna el ID de la cuenta a la transacción.
           MOVE Transaction-Amount TO Trans-Amount       * Asigna el monto de la transacción.
           WRITE TransactionRecord                       * Escribe el registro de la transacción en el archivo.
           CLOSE TransactionFile.                        * Cierra el archivo de transacciones.
