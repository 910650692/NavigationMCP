package com.sgm.navi.service.adapter.position.bls.comm;


import com.android.utils.log.Logger;
import com.autonavi.gbl.pos.model.LocDataType;
import com.autonavi.gbl.pos.model.LocFuncSwitch;
import com.autonavi.gbl.pos.model.LocLogConf;
import com.autonavi.gbl.pos.model.LocModeType;
import com.autonavi.gbl.pos.model.LocMountAngle;
import com.autonavi.gbl.pos.model.LocSensorOption;
import com.autonavi.gbl.pos.model.LocType;
import com.autonavi.gbl.pos.model.PlatformType;
import com.sgm.navi.service.define.position.LocMode;
import com.sgm.navi.service.define.position.PositionConfig;

/**
 * 定义了常见的定位模块功能组合
 * HMI根据具体业务需求进行修改或者增加
 */
public class LocationFuncSwitch {

    /**
     * GNSS模式常用功能
     */
    public final static int GNSS = LocFuncSwitch.LocFuncDelayTurningLowSpeed | LocFuncSwitch.LocFuncTurningMainSideRoad | LocFuncSwitch.LocFuncConfusingTurning | LocFuncSwitch.LocFuncPosDataEnable |   //巡航
            LocFuncSwitch.LocFuncReroutingRejectorPassOver | LocFuncSwitch.LocFuncReroutingRejectorARS | LocFuncSwitch.LocFuncReroutingRejectorDist |           // 导航偏航抑制器
            LocFuncSwitch.LocFuncReroutingByCruiseTunnelCorrection; //导航辅助偏航插件

    /**
     * DR前端融合模式默认功能组合
     */
    public final static int DR_FRONT_DEFAULT = LocFuncSwitch.LocFuncConfusingTurning | LocFuncSwitch.LocFuncMainSideRoadDecision | LocFuncSwitch.LocFuncForkQuickCorrection | LocFuncSwitch.LocFuncPosDataEnable     // 巡航
            | LocFuncSwitch.LocFuncReroutingRejectorPassOver | LocFuncSwitch.LocFuncReroutingRejectorARS | LocFuncSwitch.LocFuncReroutingRejectorDist;   // 导航

    /**
     * 后端融合(三轴陀螺+三轴加速度计)(0xf39ba或者997818)
     */
    public final static int DR_BACK_DEFAULT = LocFuncSwitch.LocFuncReroutingByCruiseViaduct | LocFuncSwitch.LocFuncViaductRecognize
            | LocFuncSwitch.LocFuncParallelRecognize | LocFuncSwitch.LocFuncDivergingPathsRecognize | LocFuncSwitch.LocFuncTurningSmooth   // 巡航
            | LocFuncSwitch.LocFuncTurningMainSideRoad | LocFuncSwitch.LocFuncUTurnMatch | LocFuncSwitch.LocFuncAbsolutePosCorrection   // 巡航
            | LocFuncSwitch.LocFuncTunnelCorrection | LocFuncSwitch.LocFuncLeaveRoundabout | LocFuncSwitch.LocFuncPosDataEnable          // 巡航
            | LocFuncSwitch.LocFuncReroutingRejectorPassOver | LocFuncSwitch.LocFuncReroutingRejectorARS | LocFuncSwitch.LocFuncReroutingRejectorDist    // 导航
            | LocFuncSwitch.LocFuncReroutingRejectorCross  | LocFuncSwitch.LocFuncEHPEnable;
    // 2.后端融合模式（单轴陀螺或三轴陀螺）(0xf39b0或者997808)
                /*locModeType.funcs |= LocFuncSwitch.LocFuncDivergingPathsRecognize | LocFuncSwitch.LocFuncTurningSmooth | LocFuncSwitch.LocFuncTurningMainSideRoad                 // 巡航
                        | LocFuncSwitch.LocFuncUTurnMatch | LocFuncSwitch.LocFuncTunnelCorrection | LocFuncSwitch.LocFuncLeaveRoundabout | LocFuncSwitch.LocFuncPosDataEnable              // 巡航
                        | LocFuncSwitch.LocFuncReroutingRejectorPassOver | LocFuncSwitch.LocFuncReroutingRejectorARS | LocFuncSwitch.LocFuncReroutingRejectorDist   // 导航
                        | LocFuncSwitch.LocFuncReroutingRejectorCross;*/

    /**
     * 获取LocModeType，根据项目传入的定位类型进行不同定位模式实现类初始化
     */
    public static LocModeType getLocModeType(LocMode locType, PositionConfig positionConfig) {
        // 定位工作模式
        LocModeType locModeType = new LocModeType();
        switch (locType) {
            case GNSS: // 纯GPS定位模式
                // 功能插件，设置要启用的决策插件类型，该值为枚举LocFuncSwitch的位运算组合。
                // GNSS模式（0x8724c0或者8856768）
                locModeType.locType = LocType.LocTypeGNSS;
                locModeType.funcs = LocationFuncSwitch.GNSS;
                break;
            case DrBack: // DR模式(后端融合)
                Logger.d("PositionBlsStrategy", "DR模式(后端融合) start !");
                // 信号类型，设置后端融合模式下信号组合方式，该值为枚举LocDataType的位运算组合，引擎支持的组合方式参见LocSignalCombine。（当LocType == LocTypeDrBack时为必传参数，否则默认为0）
                locModeType.locType = LocType.LocTypeDrBack;
                locModeType.signalTypes =
                        LocDataType.LocDataAcce3D | LocDataType.LocDataAirPressure | LocDataType.LocDataGnss |
                                LocDataType.LocDataECompass | LocDataType.LocDataGpgsv | LocDataType.LocDataGyro | LocDataType.LocDataPulse;
                // 功能插件，设置要启用的决策插件类型，该值为枚举LocFuncSwitch的位运算组合。
                // 1.后端融合(三轴陀螺+三轴加速度计)(0xf39ba或者997818)
                locModeType.funcs = LocationFuncSwitch.DR_BACK_DEFAULT;
                locModeType.sensorOption = getSensorOption(positionConfig);
                locModeType.mountAngle = getMountAngle(positionConfig);
                break;
            case DrFront: // DR模式(前端融合)
                // 功能插件，设置要启用的决策插件类型，该值为枚举LocFuncSwitch的位运算组合。
                // 前端融合模式(0x72604或者468484)
                locModeType.locType = LocType.LocTypeDrFront;
                locModeType.funcs = LocationFuncSwitch.DR_FRONT_DEFAULT;
                break;
        }
        //定位模块云+端功能是否开启,一般情况下有EHP项目才需要开启
//        if (AutoConstant.isOpenLocationEhp) {
//            locModeType.funcs |= LocFuncSwitch.LocFuncEHPEnable;
//        }
        locModeType.platformType = PlatformType.PlatformAuto;
        locModeType.logConf = getLocLogConf(); // 配置日志参数
        return locModeType;
    }

    public static LocLogConf getLocLogConf() {
        LocLogConf locConfig = new LocLogConf();
        int spaceMaxLimit = 240;
        int fileMaxLimit = 60;
        locConfig.spaceLimit = spaceMaxLimit;
        locConfig.fileLimit = fileMaxLimit;
        return locConfig;
    }

    /**
     * 传感器参数，设置后端融合模式下传感器相关参数（当LocType == LocTypeDrBack时为必传参数，否则默认不传）
     */
    private static LocSensorOption getSensorOption(PositionConfig positionConfig) {
        LocSensorOption sensorOption = new LocSensorOption();
        sensorOption.hasAcc = positionConfig.getHasAcc();//加速度计轴数 {0|1|3}
        sensorOption.hasGyro = positionConfig.getHasGyro();// 陀螺仪轴数 {0|1|3}
        sensorOption.hasTemp = positionConfig.getHasTemp();// 有无陀螺温度传感器  0无 1有
        sensorOption.hasPressure = positionConfig.getHasPressure();
        sensorOption.hasMag = positionConfig.getHasMag();
        sensorOption.hasW4m = positionConfig.getHasW4m();
        sensorOption.hasGsv = positionConfig.getHasGsv();// 有无GSV信息（星历信息）， 0无 1有 TODO :根据项目情况配置，推荐1hz，后端融合项目必须有
        sensorOption.pulseFreq = positionConfig.getPulseFreq(); // 脉冲信息输入频率，单位 Hz TODO :根据项目情况配置，推荐10hz，后端融合项目必须有
        sensorOption.gyroFreq = positionConfig.getGyroFreq(); // 陀螺仪信息输入频率，单位 Hz TODO :根据项目情况配置，推荐10hz，后端融合项目必须有
        sensorOption.gpsFreq = positionConfig.getGpsFreq(); // GNSS信息输入频率，单位 Hz TODO :根据项目情况配置，推荐1hz，后端融合项目必须有
        sensorOption.accFreq = positionConfig.getAccFreq(); // 加速度计信息输入频率，单位 Hz TODO :根据项目情况配置，推荐10hz，后端融合项目可选
        sensorOption.w4mFreq = positionConfig.getW4mFreq();
        return sensorOption;
    }

    /**
     * 安装角，设置后端融合模式下设备安装角信息。（当LocType == LocTypeDrBack时为必传参数，否则默认不传）。
     */
    private static LocMountAngle getMountAngle(PositionConfig positionConfig) {
        LocMountAngle locMountAngle = new LocMountAngle();
        locMountAngle.isValid = positionConfig.isValid(); // TODO： 安装角是否可用，需根据项目情况正确配置
        locMountAngle.yaw = positionConfig.getYaw();   // TODO： 安装角yaw值，需根据项目情况正确配置
        locMountAngle.roll = positionConfig.getRoll();  // TODO： 安装角roll值，需根据项目情况正确配置
        locMountAngle.pitch = positionConfig.getPitch(); // TODO： 安装角pitch值，需根据项目情况正确配置
        return locMountAngle;
    }
}
