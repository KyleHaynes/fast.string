#ifndef FAST_STRING_SEQUENCE_INPUT_H
#define FAST_STRING_SEQUENCE_INPUT_H

#include <Rcpp.h>

#include "codepoint_snapshot.h"
#include "rapidfuzz_core.h"
#include "string_snapshot.h"

#include <cstddef>
#include <memory>
#include <vector>

// One string as code units: code points, or bytes with use_bytes. Workers
// copy into a per-thread buffer, so the metrics see one representation.
struct SequenceSource {
    const StringView* bytes;
    const CodepointView* points;  // null with use_bytes

    bool missing(std::size_t i) const { return bytes[i].is_na(); }

    const rf_char* get(std::size_t i, std::vector<rf_char>& buffer,
                       int& size) const {
        if (points) {
            size = static_cast<int>(points[i].size);
            return points[i].data;
        }
        const StringView& v = bytes[i];
        if (buffer.size() < v.size) buffer.resize(v.size);
        for (std::size_t k = 0; k < v.size; ++k)
            buffer[k] = static_cast<unsigned char>(v.data[k]);
        size = static_cast<int>(v.size);
        return buffer.data();
    }
};

// Holds the snapshots behind a SequenceSource.
struct SequenceInput {
    StringSnapshot bytes;
    std::unique_ptr<CodepointSnapshot> points;

    SequenceInput(const Rcpp::StringVector& x, bool use_bytes, const char* name)
        : bytes(x, use_bytes ? SnapshotText::raw : SnapshotText::utf8) {
        if (!use_bytes) points.reset(new CodepointSnapshot(x, name));
    }

    SequenceSource source() const {
        return SequenceSource{bytes.data(), points ? points->data() : nullptr};
    }
};

#endif  // FAST_STRING_SEQUENCE_INPUT_H
