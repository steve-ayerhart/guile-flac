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

static SCM scm_make_instance;

// helper macros

#define FLAC_METADATA_GET_INSTANCE(class) (FLAC__StreamMetadata *) SCM_STRUCT_DATA_REF (class, 0)

SCM_GLOBAL_SYMBOL (scm_flac_sym_type, "type");

SCM_KEYWORD (scm_flac_kw_type, "type");

static void
scm_flac_metadata_struct_free (SCM scm_flac_metadata)
{
  FLAC__StreamMetadata *stream_metadata;

  stream_metadata = (FLAC__StreamMetadata *) SCM_STRUCT_DATA_REF (scm_flac_metadata, 0);

  if (stream_metadata)
    {
      FLAC__metadata_object_delete (stream_metadata);
      // TODO: call scm_gc_free?
    }
}

SCM_DEFINE (scm_protect_stream_metadata_class, "%protect-stream-metadata-class!", 1, 0, 0,
            (SCM scm_metadata_class),
            "")
{
  scm_t_bits *slots = SCM_STRUCT_DATA (scm_metadata_class);
  SCM scm_class_metadata_class = scm_permanent_object (scm_variable_ref (scm_c_lookup ("<stream-metadata>")));

  slots[scm_vtable_index_instance_finalize] = (scm_t_bits) scm_flac_metadata_struct_free;

  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_initialize_stream_metadata_class, "%initialize-stream-metadata-class!", 2, 0, 0,
           (SCM scm_metadata_class, SCM scm_args),
           "")
{
  SCM scm_type = SCM_UNDEFINED;

  scm_c_bind_keyword_arguments ("initialize", scm_args, SCM_ALLOW_OTHER_KEYS,
                                scm_flac_kw_type, &scm_type,
                                SCM_UNDEFINED);

  if (SCM_UNBNDP (scm_type))
    scm_misc_error ("initialize", "fart", SCM_EOL);

  scm_slot_set_x (scm_metadata_class, scm_flac_sym_type, scm_type);

  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_allocate_flac_metadata, "%allocate-stream-metadata-class", 2, 0, 0,
           (SCM scm_metadata_class, SCM scm_metadata_instance),
           "")
{
  FLAC__StreamMetadata *stream_metadata;
  FLAC__MetadataType metadata_type;

  metadata_type = scm_to_int (scm_slot_ref (scm_metadata_class, scm_flac_sym_type));
  stream_metadata = FLAC__metadata_object_new (metadata_type);

  SCM_STRUCT_DATA (scm_metadata_instance)[0] = (scm_t_bits) stream_metadata;

  return scm_metadata_instance;
}

// STREAMINFO

SCM_DEFINE (scm_stream_info_get_min_blocksize, "%stream-info-get-min-block-size", 1, 0, 0,
            (SCM scm_streaminfo),
            "")
{
  FLAC__StreamMetadata *stream_metadata = FLAC_METADATA_GET_INSTANCE (scm_streaminfo);
  return scm_from_uint (stream_metadata->data.stream_info.min_blocksize);
}

SCM_DEFINE (scm_stream_info_get_max_blocksize, "%stream-info-get-max-block-size", 1, 0, 0,
            (SCM scm_streaminfo),
            "")
{
  FLAC__StreamMetadata *stream_metadata = FLAC_METADATA_GET_INSTANCE (scm_streaminfo);
  return scm_from_uint (stream_metadata->data.stream_info.max_blocksize);
}

SCM_DEFINE (scm_stream_info_get_min_framesize, "%stream-info-get-min-frame-size", 1, 0, 0,
            (SCM scm_streaminfo),
            "")
{
  FLAC__StreamMetadata *stream_metadata = FLAC_METADATA_GET_INSTANCE (scm_streaminfo);
  return scm_from_uint (stream_metadata->data.stream_info.min_framesize);
}

SCM_DEFINE (scm_stream_info_get_max_framesize, "%stream-info-get-max-frame-size", 1, 0, 0,
            (SCM scm_streaminfo),
            "")
{
  FLAC__StreamMetadata *stream_metadata = FLAC_METADATA_GET_INSTANCE (scm_streaminfo);
  return scm_from_uint (stream_metadata->data.stream_info.max_framesize);
}

SCM_DEFINE (scm_stream_info_get_sample_rate, "%stream-info-get-sample-rate", 1, 0, 0,
            (SCM scm_streaminfo),
            "")
{
  FLAC__StreamMetadata *stream_metadata = FLAC_METADATA_GET_INSTANCE (scm_streaminfo);
  return scm_from_uint (stream_metadata->data.stream_info.sample_rate);
}

SCM_DEFINE (scm_stream_info_get_channels, "%stream-info-get-channels", 1, 0, 0,
            (SCM scm_streaminfo),
            "")
{
  FLAC__StreamMetadata *stream_metadata = FLAC_METADATA_GET_INSTANCE (scm_streaminfo);
  return scm_from_uint (stream_metadata->data.stream_info.channels);
}

SCM_DEFINE (scm_stream_info_get_bits_per_sample, "%stream-info-get-bits-per-sample", 1, 0, 0,
            (SCM scm_streaminfo),
            "")
{
  FLAC__StreamMetadata *stream_metadata = FLAC_METADATA_GET_INSTANCE (scm_streaminfo);
  return scm_from_uint (stream_metadata->data.stream_info.bits_per_sample);
}

SCM_DEFINE (scm_stream_info_get_total_sample, "%stream-info-get-total-samples", 1, 0, 0,
            (SCM scm_streaminfo),
            "")
{
  FLAC__StreamMetadata *stream_metadata = FLAC_METADATA_GET_INSTANCE (scm_streaminfo);
  return scm_from_uint64 (stream_metadata->data.stream_info.total_samples);
}

SCM_DEFINE (scm_stream_info_get_md5sum, "%stream-info-get-md5sum", 1, 0, 0,
            (SCM scm_streaminfo),
            "")
{
  FLAC__StreamMetadata *stream_metadata = FLAC_METADATA_GET_INSTANCE (scm_streaminfo);
  return scm_pointer_to_bytevector (scm_from_pointer ((void *) stream_metadata->data.stream_info.md5sum, NULL),
                                    scm_from_uint(16),
                                    scm_from_uint(0),
                                    scm_from_utf8_symbol ("u8"));
}

SCM_DEFINE (scm_metadata_get_streaminfo, "%flac-metadata-get-stream-info", 1, 0, 0,
            (SCM scm_path),
            "")
{
  FLAC__StreamMetadata *stream_metadata;
  FLAC__bool status;
  char *path;
  SCM scm_stream_info_instance;

  scm_dynwind_begin (0);

  scm_stream_info_instance = scm_call_1 (scm_make_instance,
                                         scm_variable_ref (scm_c_lookup ("<stream-info>")));
  stream_metadata = (FLAC__StreamMetadata *) SCM_STRUCT_DATA_REF (scm_stream_info_instance, 0);
  path = scm_to_locale_string (scm_path);

  scm_dynwind_free (path);

  status = FLAC__metadata_get_streaminfo (path, stream_metadata);

  scm_dynwind_end ();

  if (status == false)
    scm_error (scm_from_locale_symbol ("memory-allocation-error"),
               NULL,
               NULL,
               SCM_EOL, SCM_EOL);


  SCM_STRUCT_DATA (scm_stream_info_instance)[0] = (scm_t_bits) stream_metadata;

  return scm_stream_info_instance;
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

  // (oop goops)
  scm_make_instance = scm_permanent_object (scm_variable_ref (scm_c_lookup ("make-instance")));


}
