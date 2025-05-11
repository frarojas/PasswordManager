# PasswordManager

Este proyecto es una aplicación para la gestión segura de contraseñas, utilizando cifrado y almacenamiento seguro de información. Permite registrar, almacenar y gestionar contraseñas de forma segura.

## Requisitos

Para ejecutar este proyecto, necesitas tener instalados los siguientes programas:

- [Haskell](https://www.haskell.org/)
- [Stack](https://docs.haskellstack.org/en/stable/README/) para la gestión de dependencias y construcción del proyecto

## Instalación

1. **Clona el repositorio**:
   ```bash
   git clone https://github.com/tu_usuario/PasswordManager.git
   cd PasswordManager
   ```

2. **Instalacion de dependencias**: 
   ```bash
   stack setup
   stack build
   stack exec PasswordManager
   ```

3. **Estructura actual del proyecto**:
    ```pgsql
    PasswordManager/
    ├── app/
    │   └── Main.hs                 -- Punto de entrada principal
    ├── src/
    │   ├── User.hs                -- Registro, validación y acceso por PIN
    │   ├── Password.hs            -- CRUD de contraseñas
    │   ├── Storage.hs             -- Lectura/escritura de archivos cifrados
    │   ├── Encryption.hs          -- Funciones de cifrado y descifrado
    │   └── Utils.hs               -- Funciones auxiliares (ocultamiento de entrada, etc.)
    ├── test/
    │   └── Tests.hs               -- Pruebas del sistema (opcional)
    ├── README.md                  -- Instrucciones del proyecto
    ├── LICENSE                    -- Licencia (opcional)
    ├── stack.yaml                 -- Configuración del proyecto con Stack
    └── .gitignore                 -- Ignorar archivos sensibles
    ```