package com.fy.navi.service.define.position;

import java.io.Serializable;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class SensorCalibrationPara implements Serializable {
    private long startTime = -1;
    private long endTime = -1;
    private String para = "首次刷机，未开始标定";

    public SensorCalibrationPara(String para, long startTime, long endTime) {
        this.para = para;
        this.startTime = startTime;
        this.endTime = endTime;
    }
}
