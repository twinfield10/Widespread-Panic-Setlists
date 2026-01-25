# AWS Credentials Setup

**NEVER commit AWS credentials to git!**

## Setup Instructions

### Option 1: R Environment File (Recommended)

1. Create a `.Renviron` file in the project root:
   ```bash
   touch .Renviron
   ```

2. Add your AWS credentials:
   ```
   AWS_ACCESS_KEY_ID=your_access_key_here
   AWS_SECRET_ACCESS_KEY=your_secret_key_here
   AWS_DEFAULT_REGION=us-east-1
   AWS_S3_BUCKET=wsmfp-setlists
   ```

3. Restart R session to load the environment variables

### Option 2: System Environment Variables

Set environment variables in your shell profile (`~/.zshrc` or `~/.bash_profile`):

```bash
export AWS_ACCESS_KEY_ID="your_access_key_here"
export AWS_SECRET_ACCESS_KEY="your_secret_key_here"
export AWS_DEFAULT_REGION="us-east-1"
export AWS_S3_BUCKET="wsmfp-setlists"
```

## Verify Setup

In R, check that credentials are loaded:

```r
Sys.getenv("AWS_ACCESS_KEY_ID")
Sys.getenv("AWS_SECRET_ACCESS_KEY")
```

## Security Notes

- `.Renviron` is in `.gitignore` and will NOT be committed
- Never hardcode credentials in R scripts
- Use `Sys.getenv()` to read credentials at runtime
