kernel = DeltaFunction[x1, x2] DeltaFunction[x3, x4] V[x1, x3];
BetheSalpeterLadder[kernel, kernel, {x1, x2, x3, x4}, "ParticleHole", G]