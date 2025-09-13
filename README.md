# Complexify

Complexify is a web application that takes a simple mathematical equation and transforms it into a more complex equivalent, then evaluates the result. Built with F#, Bolero, and Elmish, it provides an interactive UI for exploring mathematical complexity.

## Features
- Input a simple equation (e.g., `2+2=4`).
- Adjust the complexity level using a slider.
- View the transformed ("complexified") equation.
- See the evaluation result of the complexified equation.

## Technologies Used
- **F#**: Functional programming language for .NET.
- **Bolero**: F# web framework for Blazor WebAssembly.
- **Elmish**: Model-View-Update architecture for state management.
- **FParsec**: Parser combinator library for F#.

## Project Structure
- `src/Complexify.Client/Parser.fs`: Parses mathematical expressions using FParsec, supporting operators like +, -, *, /, ^, =, log, and exp.
- `src/Complexify.Client/Runner.fs`: Evaluates parsed expressions and generates more complex equivalents.
- `src/Complexify.Client/Main.fs`: Implements the Elmish model, update logic, and view bindings for the UI.
- `src/Complexify.Client/Startup.fs`: Configures and starts the Blazor WebAssembly host.
- `src/Complexify.Client/wwwroot/main.html`: Defines the main UI layout and bindings for equation input, complexity slider, and output display.

## Getting Started
1. **Prerequisites**:
   - [.NET SDK](https://dotnet.microsoft.com/download)
   - [Node.js](https://nodejs.org/) (for frontend tooling, if needed)
2. **Build and Run**:
   - Navigate to the project directory:
     ```sh
     cd src/Complexify.Client
     dotnet run
     ```
   - Open your browser at the provided local address (usually `http://localhost:5000`).

## Usage
- Enter a mathematical equation in the input field.
- Use the complexity slider to increase or decrease the complexity of the generated equation.
- The new equation and its evaluation result will be displayed below.

## License
This project is licensed under the MIT License.

## Acknowledgements
- [Bolero](https://fsbolero.io/)
- [Elmish](https://elmish.github.io/elmish/)
- [FParsec](https://www.quanttec.com/fparsec/)
