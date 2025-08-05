package com.sgm.navi.service.define.navi;

import androidx.annotation.NonNull;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import lombok.Getter;
import lombok.Setter;


public class NaviDriveReportEntity {
    // 起点
    @Getter
    @Setter
    private String startPos;
    // 终点
    @Getter
    @Setter
    private String endPos;
    // 驾驶时长
    @Getter
    @Setter
    private int drivenTime;
    // 驾驶里程
    @Getter
    @Setter
    private int drivenDist;
    // 预计每百公里油耗
    @Getter
    @Setter
    private int estimateFuelConsume;
    // 累计油耗
    @Getter
    @Setter
    private int totalFuelConsume;

    @NonNull
    @Override
    public String toString() {
        return "NaviDriveReportEntity{" +
                "startPos='" + startPos + '\'' +
                ", endPos='" + endPos + '\'' +
                ", drivenTime=" + drivenTime +
                ", drivenDist=" + drivenDist +
                ", estimateFuelConsume=" + estimateFuelConsume +
                ", totalFuelConsume=" + totalFuelConsume +
                '}';
    }
}
