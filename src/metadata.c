#include "metadata.h"

#include <libguile.h>
#include <FLAC/format.h>
#include <FLAC/metadata.h>

/*
  (define max-metadata-type-code 126)
  (define min-block-size 16)
  (define max-block-size 65535)
  (define subset-max-block-size 4608)
  (define max-channels 8)
  (define min-bits-per-sample 4)
  (define max-bits-per-sample 32)
  (define reference-codec-max-bits-per-sample 24)
  (define max-sample-rate 655350)
  (define max-lpc-order 32)
  (define subset-max-lpc-order-4800hz 12)
  (define min-qlp-coeff-precision 5)
  (define max-qlp-coeff-precision 15)
  (define max-fixed-order 4)
  (define max-rice-partition-order 15)
  (define subset-max-rice-partition-order 8)
*/

SCM_DEFINE(scm_allocate_flac_metadata, "%allocate-flac-metadata!", 1, 0, 0,
           (SCM scm_metadata, SCM scm_metadata_type),
           "")
{
  FLAC__StreamMetadata *stream_metadata;
  FLAC__MetadataType metadata_type;

  metadata_type = (FLAC__MetadataType) scm_to_int (scm_metadata_type);
  stream_metadata = FLAC__metadata_object_new (metadata_type);

  scm_foreign_object_set_x (scm_metadata_class, 0, (void *) stream_metadata);

  return scm_metadata;
}

SCM_DEFINE(scm_finalize_flac_metadata, "%finalize-flac-metadata!", 1, 0, 0,
           (SCM scm_class),
           "")
{
  return SCM_UNDEFINED;
}

void
guile_flac_metadata_init (void)
{
  #ifndef SCM_MAGIC_SNARFER
  #include "metadata.x"
  #endif

  scm_c_define ("flac-version", scm_from_locale_string (FLAC__VERSION_STRING));
  scm_c_define ("flac-vendor", scm_from_locale_string (FLAC__VENDOR_STRING));


  scm_c_export ("flac-version",
                "flac-vendor",
                NULL);
}
