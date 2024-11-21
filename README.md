# cosmic_ray_simulation_with_fluka
cosmic_ray_simulation_with_fluka
This workflow is designed to process and analyze secondary cosmic ray data generated using FLUKA. FLUKA is a trademarked Monte Carlo simulation software developed and maintained by the FLUKA team. For more information, please visit (https://www.fluka.org/).

step1_primary: Record information on primary cosmic rays in the outermost layers of the atmosphere, mainly using the fluscw.f card.
step1_surface: Similar to the first step, but recording information on cosmic rays reaching the Earth.
step2: Use the source.f file to sample the particle information in the phase file as a source to interact with the scintillator

The additional folder provides some handy python scripts for dealing with cosmic rays.
