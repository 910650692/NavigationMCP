package com.fy.navi.service.define.position;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * 传感器参数对象 后端融合必传参数.
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class PositionConfig {
    private int hasAcc = 3;// 加速度计轴数 {0|1|3}， 0 表示没有 后端融合项目可选
    private int hasGyro = 3; // 陀螺仪轴数 {0|1|3} ， 0 表示没有 后端融合项目必须有
    private int hasTemp = 1;// 有无陀螺温度传感器  0无 1有 后端融合项目必须有
    private int hasPressure = 0;// 有无气压计  0无 1有， 一般可不配置
    private int hasMag = 0;// 有无磁力计  0无 1有， 一般可不配置
    private int hasW4m = 0;// 有无四轮速传感器  0无 1有 未使用，可不配置
    private int hasGsv = 1;// 有无GSV信息（星历信息）， 0无 1有 TODO :根据项目情况配置，推荐1hz，后端融合项目必须有
    private int pulseFreq = 10;// 脉冲信息输入频率，单位 Hz TODO :根据项目情况配置，推荐10hz，后端融合项目必须有
    private int gyroFreq = 10;// 陀螺仪信息输入频率，单位 Hz TODO :根据项目情况配置，推荐10hz，后端融合项目必须有
    private int gpsFreq = 1;// GNSS信息输入频率，单位 Hz TODO :根据项目情况配置，推荐1hz，后端融合项目必须有
    private int accFreq = 10;// 加速度计信息输入频率，单位 Hz TODO :根据项目情况配置，推荐10hz，后端融合项目可选
    private int w4mFreq = 10;//四轮速信息输信息入频率，单位 Hz 未使用，可不配置
    private boolean isValid = true;// TODO： 安装角是否可用，需根据项目情况正确配置
    private double yaw = 0;// TODO： 安装角yaw值，需根据项目情况正确配置
    private double pitch = 0;// TODO： 安装角roll值，需根据项目情况正确配置
    private double roll = 0;// TODO： 安装角pitch值，需根据项目情况正确配置

    @Override
    public String toString() {
        return "PositionConfig{" +
                "hasAcc=" + hasAcc +
                ", hasGyro=" + hasGyro +
                ", hasTemp=" + hasTemp +
                ", hasPressure=" + hasPressure +
                ", hasMag=" + hasMag +
                ", hasW4m=" + hasW4m +
                ", hasGsv=" + hasGsv +
                ", pulseFreq=" + pulseFreq +
                ", gyroFreq=" + gyroFreq +
                ", gpsFreq=" + gpsFreq +
                ", accFreq=" + accFreq +
                ", w4mFreq=" + w4mFreq +
                ", isValid=" + isValid +
                ", yaw=" + yaw +
                ", pitch=" + pitch +
                ", roll=" + roll +
                '}';
    }
}
