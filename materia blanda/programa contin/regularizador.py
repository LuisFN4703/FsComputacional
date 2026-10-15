import argparse
import sys
from typing import Tuple, List

import numpy as np

def leer_txt(path: str) -> Tuple[str, np.ndarray, np.ndarray]:
    with open(path, 'r', encoding='utf-8') as f:
        lines = [ln.rstrip('\n') for ln in f if ln.strip() != '']
    if len(lines) < 2:
        raise ValueError("El archivo debe tener al menos una línea de título y una línea de datos.")
    titulo = lines[0]
    xs = []
    ys = []
    for i, ln in enumerate(lines[1:], start=2):
        parts = ln.split('\t')
        if len(parts) < 2:
            raise ValueError(f"Línea {i} mal formada: '{ln}' (se esperan x y separados por tab).")
        try:
            x = float(parts[0].strip())
            y = float(parts[1].strip())
        except ValueError:
            raise ValueError(f"Línea {i} contiene valores no numéricos: '{ln}'")
        xs.append(x)
        ys.append(y)
    xs = np.array(xs, dtype=float)
    ys = np.array(ys, dtype=float)
    # Ordenar por x por si vienen desordenados
    order = np.argsort(xs)
    xs = xs[order]
    ys = ys[order]
    return titulo, xs, ys

def regularizar_lineal(xs: np.ndarray, ys: np.ndarray, N: int) -> Tuple[np.ndarray, np.ndarray]:
    if N < 2:
        raise ValueError("N debe ser al menos 2 (primer y último punto).")
    x0, xN = xs[0], xs[-1]
    x_new = np.linspace(x0, xN, N)
    # Interpolación lineal; np.interp preserva los extremos
    y_new = np.interp(x_new, xs, ys)
    return x_new, y_new

def escribir_txt(path: str, titulo: str, xs: np.ndarray, ys: np.ndarray) -> None:
    with open(path, 'w', encoding='utf-8') as f:
        f.write(titulo + '\n')
        for x, y in zip(xs, ys):
            f.write(f"{x}\t{y}\n")

def main():
    parser = argparse.ArgumentParser(description="Regularizar datos por interpolación lineal y muestreo equiespaciado.")
    parser.add_argument('input', help='Archivo de entrada (.txt)')
    parser.add_argument('output', help='Archivo de salida (.txt)')
    parser.add_argument('--N', type=int, required=True, help='Número total de puntos deseados (incluye primer y último)')
    parser.add_argument('--plot', action='store_true', help='Mostrar gráfico comparativo (requiere matplotlib)')
    args = parser.parse_args()

    try:
        titulo, xs, ys = leer_txt(args.input)
    except Exception as e:
        print(f"Error leyendo archivo: {e}", file=sys.stderr)
        sys.exit(1)

    original_count = len(xs)
    if args.N > original_count:
        print(f"ADVERTENCIA: solicitaste {args.N} puntos, el archivo original tiene {original_count} puntos. Se generarán más puntos que los originales.", file=sys.stderr)

    try:
        x_new, y_new = regularizar_lineal(xs, ys, args.N)
    except Exception as e:
        print(f"Error en regularización: {e}", file=sys.stderr)
        sys.exit(1)

    try:
        escribir_txt(args.output, titulo, x_new, y_new)
    except Exception as e:
        print(f"Error escribiendo archivo de salida: {e}", file=sys.stderr)
        sys.exit(1)

    print(f"Salida escrita en '{args.output}'. Puntos: {len(x_new)} (originales: {original_count}).")

    if args.plot:
        try:
            import matplotlib.pyplot as plt
            plt.figure(figsize=(8,5))
            plt.plot(xs, ys, 'o-', label='Original', markersize=6)
            plt.plot(x_new, y_new, 'x--', label='Regularizado (equispaciado)', markersize=4)
            plt.xlabel('x')
            plt.ylabel('y')
            plt.title(titulo)
            plt.legend()
            plt.grid(True)
            plt.show()
        except Exception as e:
            print(f"No se pudo mostrar el gráfico (matplotlib requerido): {e}", file=sys.stderr)

if __name__ == '__main__':
    main()
