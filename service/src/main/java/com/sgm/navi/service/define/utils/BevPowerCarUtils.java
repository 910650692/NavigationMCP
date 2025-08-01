package com.sgm.navi.service.define.utils;

import com.android.utils.gson.GsonUtils;
import com.sgm.navi.service.define.bevpower.ElecCommonParameter;
import com.sgm.navi.service.define.bevpower.ElecSpeedCostList;
import com.sgm.navi.service.define.bevpower.PowertrainLoss;
import com.google.gson.reflect.TypeToken;
import com.android.utils.ScreenType;

import java.util.ArrayList;
import java.util.List;

public class BevPowerCarUtils {

    public String engineVersion = "1.0";
    public String sdkVersion = "750";
    public double batterToDistance = 4200;//每1%的电行驶多少公里,离线状态下预估值
    public double batterToDistanceCarSignal = 0;//每1%的电行驶多少公里，根据车身信号计算
    public boolean isElecPlanRoute = true; //是否自动续航-路线添加充电站 ---接口获取
    public String carType = "2"; //车辆类型：0客车，1货车, 2电动客车，3电动货车，4插电式混动客车，5插电式混动货车---接口获取
    public boolean bevCarElicOpen = true; //电车设置能耗模型
    public  float maxBattenergy = 90.f;//电车最大电量---接口获取
    public  short vehicleWeight = 1440;//车重 kg---接口获取
    public String extraBrand = "Buick";//品牌---接口获取
    public String vehicleType = "B233";//车型代号---接口获取
    public short energyUnit = 1; //1: KWH--写死
    public float initlialHVBattenergy = 70.f;//电车当前电量---接口获取
    public short curDriveMode = 0; //无信号--写死
    public int charging = 0;// 0:未充电 1:充电中---接口获取
    public int temperature = 26; //外界温度-摄氏度---接口获取
    public int chargingPower = 150; //充电功率 KW--写死
    public int arrivingPercent = 90; //离开充电站的电量至少--写死
    public int leavingPercent = 20;//进入充电站/到达终点的电量至少--写死

    //代价模型
    public boolean airConditioningOpen = true; //空调 true:1.0f false:0.5f---接口获取
    public float ferryRate = 0.2f; //轮渡消耗--写死
    public ScreenType screenType = ScreenType.SCREEN_FULL; //屏幕属性
    public List<ElecSpeedCostList> elecSpeedCostLists = GsonUtils.fromJson2List("[{\"costValue\":17.5,\"speed\":20},{\"costValue\":37.5,\"speed\":60},{\"costValue\":67.5,\"speed\":80},{\"costValue\":94.5,\"speed\":100},{\"costValue\":134.5,\"speed\":120}]"
            , new TypeToken<ArrayList<ElecSpeedCostList>>(){}.getType()); //代价模型_速度 ---写死
    public List<PowertrainLoss> powertrainLoss =  GsonUtils.fromJson2List("[{\"costValue\":3.5,\"powerdemand\":7.0},{\"costValue\":8.5,\"powerdemand\":15.0}]"
            , new TypeToken<List<PowertrainLoss>>(){}.getType()); // 代价模型_传动---写死
    public ElecCommonParameter trans = new ElecCommonParameter(1800000.0f, 3200000.0f);//代价模型_转向---写死
    public ElecCommonParameter curve = new ElecCommonParameter(1800000.0f, 3200000.0f);//代价模型_弯道---写死
    public ElecCommonParameter slope = new ElecCommonParameter(1200000.0f, 3200000.0f); //代价模型_坡度---写死

    public boolean isLongRoute = false;

    public static BevPowerCarUtils getInstance() {
        return Helper.carU;
    }

    private static class Helper {
        private static final BevPowerCarUtils carU = new BevPowerCarUtils();
    }
}
