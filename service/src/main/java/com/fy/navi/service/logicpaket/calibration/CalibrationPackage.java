package com.fy.navi.service.logicpaket.calibration;

import com.fy.navi.service.adapter.calibration.CalibrationAdapter;
import com.fy.navi.service.define.calibration.CalibConst;

import java.util.Map;

public final class CalibrationPackage {
    public static final String TAG = CalibrationPackage.class.getSimpleName();

    private final CalibrationAdapter mCalibrationAdapter;

    public static CalibrationPackage getInstance() {
        return SInstanceHolder.INSTANCE;
    }

    private static final class SInstanceHolder {
        static final CalibrationPackage INSTANCE = new CalibrationPackage();
    }

    private CalibrationPackage() {
        mCalibrationAdapter = CalibrationAdapter.getInstance();
    }

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     * @return int
     */
    public int powerType() {
        return mCalibrationAdapter.powerType();
    }

    /**
     * 品牌标定
     * 1 Buick
     * 2 Cadillac
     * 3 Chevrolet
     * @return {@link CalibConst.Brand}
     */
    public int brand() {
        return mCalibrationAdapter.brand();
    }

    /**
     * 品牌名
     * @return String
     */
    public String brandName() {
        return switch (brand()) {
            case 1 -> "Buick";
            case 2 -> "Cadillac";
            case 3 -> "Chevrolet";
            default -> "";
        };
    }

    /**
     * 车型标定
     * 10 -> L233 // CADI
     * 11 -> L234 // CADI
     * 28 -> A2LL // CADI
     * 29, 71 -> C1UL // CADI
     * 30 -> E2QL // CADI
     * 31 -> E2UL // CADI
     * 32, 76 -> O1SL // CADI
     * 9 -> L232 // CADI
     * 20 -> U458 // BUICK
     * 55 -> U458_HEV // BUICK
     * 27 -> B233 // BUICK
     * 15 -> B223 // BUICK
     * 54 -> B234 // BUICK
     * 19 -> E2LB // BUICK
     * 13 -> E2UB // BUICK
     * 14 -> E2YB // BUICK
     * 18 -> C1YB // BUICK
     * 21 -> U358 // BUICK
     * 63, 59 -> U358_2 // BUICK
     * 57 -> NDLB // BUICK
     * 22 -> C1YC // CHEVY
     * 16 -> C223 // CHEVY
     * 24 -> D2UC_ICE // CHEVY
     * 25 -> D2UC_PHEV // CHEVY
     * @return {@link CalibConst.Model}
     */
    public int model() {
        return mCalibrationAdapter.model();
    }

    /**
     * 车型名
     * @return String
     */
    public String modelName() {
        return switch (model()) {
            case 10 -> CalibConst.Model.L233; // CADI
            case 11 -> CalibConst.Model.L234; // CADI
            case 28 -> CalibConst.Model.A2LL; // CADI
            case 29, 71 -> CalibConst.Model.C1UL; // CADI
            case 30 -> CalibConst.Model.E2QL; // CADI
            case 31 -> CalibConst.Model.E2UL; // CADI
            case 32, 76 -> CalibConst.Model.O1SL; // CADI
            case 9 -> CalibConst.Model.L232; // CADI
            case 20 -> CalibConst.Model.U458; // BUICK
            case 55 -> CalibConst.Model.U458_HEV; // BUICK
            case 27 -> CalibConst.Model.B233; // BUICK
            case 15 -> CalibConst.Model.B223; // BUICK
            case 54 -> CalibConst.Model.B234; // BUICK
            case 19 -> CalibConst.Model.E2LB; // BUICK
            case 13 -> CalibConst.Model.E2UB; // BUICK
            case 14 -> CalibConst.Model.E2YB; // BUICK
            case 18 -> CalibConst.Model.C1YB; // BUICK
            case 21 -> CalibConst.Model.U358; // BUICK
            case 63, 59 -> CalibConst.Model.U358_2; // BUICK
            case 57 -> CalibConst.Model.NDLB; // BUICK
            case 22 -> CalibConst.Model.C1YC; // CHEVY
            case 16 -> CalibConst.Model.C223; // CHEVY
            case 24 -> CalibConst.Model.D2UC_ICE; // CHEVY
            case 25 -> CalibConst.Model.D2UC_PHEV; // CHEVY
            default -> "";
        };
    }

    /**
     * LBS应用使能标定
     * false 车机系统需要在主界面和后台关闭LBS应用
     * true 车机系统需要在主界面和后台使能LBS应用
     * @return boolean
     */
    public boolean enableApplicationNavigation() {
        return mCalibrationAdapter.enableApplicationNavigation();
    }

    /**
     * 车道级导航使能标定
     * false LBS系统不支持车道级导航功能
     * true LBS系统支持车道级导航功能
     * @return boolean
     */
    public boolean laneLevelNavigatioFuncEnable() {
        return mCalibrationAdapter.laneLevelNavigatioFuncEnable();
    }

    /**
     * V2X超视距提示可视化标定
     * false LBS系统不支持V2X超视距提示可视化功能
     * true LBS系统支持V2X超视距提示可视化功能
     * @return boolean
     */
    public boolean v2xMapDisplayFuncEnable() {
        return mCalibrationAdapter.v2xMapDisplayFuncEnable();
    }

    /**
     * 限速信息源标定
     * 0 限速信息仅来自于导航
     * 1 限速信息来自于导航和ADAS Map
     * 2 限速信息来自于导航和V2X
     * 3 限速信息来自于导航、V2X和ADAS Map
     * @return int
     */
    public int speedLimitInformationSource() {
        return mCalibrationAdapter.speedLimitInformationSource();
    }

    /**
     * ADAS功能配置
     * [0x0] None, [0x1] FCM, [0x2] IDCM
     * @return int
     */
    public int adasConfigurationInfomation() {
        return mCalibrationAdapter.adasConfigurationInfomation();
    }

    /**
     * ADAS配置情况
     * 0 CLEA Arch IPCM/ACM/ADM
     * 1 CLEA Arch IPCM
     * 2 CLEA Arch ACM
     * 3 CLEA Arch ADM
     * 4 GB Arch VPM/ACP3.0/ACP3.1/ADCU
     * 5 GB Arch VPM
     * 6 GB Arch ACP3.0
     * 7 GB Arch ACP3.1
     * 8 GB Arch ADCU
     * @return int
     */
    public int adasConfigurationType() {
        return mCalibrationAdapter.adasConfigurationType();
    }

    /**
     * RSTP配置情况（扶手屏）
     * false 该车型未配置RSTP
     * true 该车型配置RSTP
     * @return boolean
     */
    public boolean rearSeatTouchPanelFuncEnable() {
        return mCalibrationAdapter.rearSeatTouchPanelFuncEnable();
    }

    /**
     * HUD配置情况
     * 0 需要关闭HUD相关功能
     * 1 需要打开HUD相关功能
     * @return int
     */
    public int hudFuncEnable() {
        return mCalibrationAdapter.hudFuncEnable();
    }

    /**
     * 偏转插件使能标定
     * false 地图不需要使能偏转插件，输入的GPS为GCJ02坐标系的数据
     * true 地图需要使能偏转插件，输入的GPS为WGS84坐标系的数据
     * @return boolean
     */
    public boolean navigationDeflectionEnable() {
        return mCalibrationAdapter.navigationDeflectionEnable();
    }

    /**
     * 架构类型区分标定
     * 0 该车型的架构为CLEA（VCU上U458车型为CLEA架构）
     * 1 该车型的架构为GB（VCU上除U458外其他车型为GB架构）
     * @return int
     */
    public int architecture() {
        return mCalibrationAdapter.architecture();
    }

    /**
     * 电池预加热数据发送开关标定
     * false 导航需要取消电池预加热的数据发送
     * true 导航需要支持电池预加热的数据发送
     * @return boolean
     */
    public boolean navigationPreConditionDataProvideEnable() {
        return mCalibrationAdapter.navigationPreConditionDataProvideEnable();
    }

    /**
     * 地图供应商区分
     * 0 导航地图为百度地图
     * 1 导航地图为高德地图
     * @return int
     */
    public int navigaitonSupplier() {
        return mCalibrationAdapter.navigaitonSupplier();
    }

    /**
     * 电车总的续航里程
     * @return int
     */
    public int highVoltageBatteryPropulsionTotalRangeNavi() {
        return mCalibrationAdapter.highVoltageBatteryPropulsionTotalRangeNavi();
    }

    /**
     * POI搜索功能标定
     * @return boolean
     */
    public boolean poiSearchFuncEnable() {
        return mCalibrationAdapter.poiSearchFuncEnable();
    }

    /**
     * 情景引擎功能标定
     * @return boolean
     */
    public boolean scenarioEngineFuncEnable() {
        return mCalibrationAdapter.scenarioEngineFuncEnable();
    }

    /**
     * 全局搜索功能标定
     * @return boolean
     */
    public boolean globalSearchFuncEnable() {
        return mCalibrationAdapter.globalSearchFuncEnable();
    }

    /**
     * 团队旅行功能标定
     * @return boolean
     */
    public boolean teamTravelFuncEnable() {
        return mCalibrationAdapter.teamTravelFuncEnable();
    }

    /**
     * 开机动画替换功能标定
     * @return boolean
     */
    public boolean bootAnimationReplacementFuncEnable() {
        return mCalibrationAdapter.bootAnimationReplacementFuncEnable();
    }

    /**
     * IME功能标定
     * @return boolean
     */
    public boolean imeFuncEnable() {
        return mCalibrationAdapter.imeFuncEnable();
    }

    /**
     * 壁纸和主题功能标定
     * @return boolean
     */
    public boolean wallpaperThemeFuncEnable() {
        return mCalibrationAdapter.wallpaperThemeFuncEnable();
    }

    /**
     * 选择默认主题值
     * 0 "EV"; 1 "Normal";2 "Avenir";3"Reversed1"; 4 "Reversed2";5 "Reversed3";
     * @return int
     */
    public int themeDefaultValue() {
        return mCalibrationAdapter.themeDefaultValue();
    }

    /**
     * 云端相关标定
     * 道路坡度上升变化对每个道路段的能量转换率
     * 车辆驾驶模式：0, 1, 2, 3, 4, 5, 6, 7
     * 能量转换率：范围从0到0.999，精度为0.001
     * @return float[]
     */
    public float[] slopeUpCostlist() {
        return mCalibrationAdapter.slopeUpCostlist();
    }

    /**
     * 云端相关标定
     * 道路坡度下降变化对每个道路段的能量转换率
     * 车辆驾驶模式：0, 1, 2, 3, 4, 5, 6, 7
     * 能量转换率：范围从0到0.999，精度为0.001
     * @return float[]
     */
    public float[] slopeDownCostlist() {
        return mCalibrationAdapter.slopeDownCostlist();
    }

    /**
     * 云端相关标定
     * Trans Access变化对每个道路段的能量转换率
     * 车辆驾驶模式：0, 1, 2, 3, 4, 5, 6, 7
     * 能量转换率：范围从0到0.999，精度为0.001
     * @return float[]
     */
    public float[] transAccessCostlist() {
        return mCalibrationAdapter.transAccessCostlist();
    }

    /**
     * 云端相关标定
     * Trans Decess变化对每个道路段的能量转换率
     * 再生制动模式：0, 1, 2, 3, 4, 5, 6, 7
     * 能量转换率：范围从0到0.999，精度为0.001
     * @return float[]
     */
    public float[] transDecessCostlist() {
        return mCalibrationAdapter.transDecessCostlist();
    }

    /**
     * 云端相关标定
     * 车辆在不同驾驶模式和速度下的能量消耗
     * 车辆驾驶模式 0, 1, 2, 3, 4, 5, 6, 7
     * 车辆速度 0, 5, 10, 15, ~ 180, 185, 190, 200 km/h
     * 能量消耗 范围从0到1000.000000，精度为0.000001瓦时/公里（watt-hour/km）
     * @return Map
     */
    public Map<Integer, Float> speedCostlist() {
        return mCalibrationAdapter.speedCostlist();
    }

    /**
     * 云端相关标定
     * 辅助负载对电气系统能量消耗的影响
     * index
     * 0 - ClimateMode_NoAction（无操作）
     * 1 - BlowerRearAuto_HeaterOn（后吹风自动加热开启）
     * 2 - BlowerFrontAuto_HeaterOn（前吹风自动加热开启）
     * 3 - BlowerTotalAuto_HeaterOn（总吹风自动加热开启）
     * 4 - BlowerRearAuto_CompOn（后吹风自动压缩机开启）
     * 5 - BlowerFrontAuto_CompOn（前吹风自动压缩机开启）
     * 6 - BlowerTotalAuto_CompOn（总吹风自动压缩机开启）
     * 7 - BlowerRearAuto_CompEcon（后吹风自动经济压缩机开启）
     * 8 - BlowerFrontAuto_CompEcon（前吹风自动经济压缩机开启）
     * 9 - BlowerTotalAuto_CompEcon（总吹风自动经济压缩机开启）
     * 10 - HighBeam_On（远光灯开启）
     * 11 - Seat Left Front: Heated Back Auto（左前座椅加热背部自动）
     * 12 - Seat Left Front: Heated Back Level 1（左前座椅加热背部等级1）
     * 13 - Seat Left Front: Heated Back Level 2（左前座椅加热背部等级2）
     * 14 - Seat Left Front: Heated Back Level 3（左前座椅加热背部等级3）
     * 15 - Seat Left Front: Heated Cushion and Back Auto（左前座椅加热垫和背部自动）
     * 16 - Seat Left Front: Heated Cushion and Back Level 1（左前座椅加热垫和背部等级1）
     * 17 - Seat Left Front: Heated Cushion and Back Level 2（左前座椅加热垫和背部等级2）
     * 18 - Seat Left Front: Heated Cushion and Back Level 3（左前座椅加热垫和背部等级3）
     * 19 - Seat Right Front: Heated Back Auto（右前座椅加热背部自动）
     * 20 - Seat Right Front: Heated Back Level 1（右前座椅加热背部等级1）
     * 21 - Seat Right Front: Heated Back Level 2（右前座椅加热背部等级2）
     * 22 - Seat Right Front: Heated Back Level 3（右前座椅加热背部等级3）
     * 23 - Seat Right Front: Heated Cushion and Back Auto（右前座椅加热垫和背部自动）
     * 24 - Seat Right Front: Heated Cushion and Back Level 1（右前座椅加热垫和背部等级1）
     * 25 - Seat Right Front: Heated Cushion and Back Level 2（右前座椅加热垫和背部等级2）
     * 26 - Seat Right Front: Heated Cushion and Back Level 3（右前座椅加热垫和背部等级3）
     * 27 - reserved（保留）
     * 28 - reserved（保留）
     * 29 - reserved（保留）
     * value
     * 额外能量消耗：范围从0到10.000000，精度为0.000001瓦时/秒（watt-hour/second）
     * @return float[]
     */
    public float[] auxCostlist() {
        return mCalibrationAdapter.auxCostlist();
    }

    /**
     * 车辆基准重量标定
     * @return int
     */
    public int vehicleWeight() {
        return mCalibrationAdapter.vehicleWeight();
    }
}