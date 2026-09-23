use clap::Parser;
use grazelang::cli::input::Cli;

fn main() {
    let cli = Cli::parse();
    std::process::exit(cli.execute());
}
