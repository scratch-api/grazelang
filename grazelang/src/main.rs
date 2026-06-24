use clap::Parser;
use grazelang::cli::input::Cli;

fn main() {
    let cli = Cli::parse();
    cli.execute();
}
