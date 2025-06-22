package com.sgm.navi.service.define.speech;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class SpeechBinaryStream {
    private byte[] buffer;
}
