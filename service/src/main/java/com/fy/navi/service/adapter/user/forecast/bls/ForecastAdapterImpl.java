package com.fy.navi.service.adapter.user.forecast.bls;

import com.android.utils.file.FileUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.forcast.ForcastService;
import com.autonavi.gbl.user.forcast.model.ArrivedType;
import com.autonavi.gbl.user.forcast.model.ForcastArrivedData;
import com.autonavi.gbl.user.forcast.model.ForcastArrivedParam;
import com.autonavi.gbl.user.forcast.model.ForcastInitParam;
import com.autonavi.gbl.user.forcast.model.OftenArrivedItem;
import com.autonavi.gbl.user.forcast.observer.IForcastServiceObserver;
import com.autonavi.gbl.util.TimeUtil;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.forecast.ForecastAdapterCallback;
import com.fy.navi.service.adapter.user.forecast.IForecastApi;
import com.fy.navi.service.define.user.forecast.ForecastArrivedDataInfo;
import com.fy.navi.service.define.user.forecast.OftenArrivedItemInfo;

import java.util.ArrayList;
import java.util.List;

/**
 * 高德 用户 - 预测服务 .
 * 对于联网同时登陆了高德账号的用户提供在线预测常去目的地的接口。
 */
public class ForecastAdapterImpl implements IForecastApi, IForcastServiceObserver {
    private static final String TAG = MapDefaultFinalTag.FORECAST_SERVICE_TAG;
    private ForcastService forecastService;
    private final List<ForecastAdapterCallback> callBacks = new ArrayList<>();

    public ForecastAdapterImpl() {
        forecastService = (ForcastService) ServiceMgr.getServiceMgrInstance().
                getBLService(SingleServiceID.ForcastSingleServiceID);
    }

    @Override
    public void initService() {
        Logger.d("initService start.");
        ForcastInitParam param = new ForcastInitParam();
        param.stCurTime = TimeUtil.getLocalTime2(); // 当前时间 com.autonavi.gbl.util.model.DateTime
        param.dbPath = GBLCacheFilePath.FORECAST_PATH;// 预测数据库文件保存目录路径
        // 保证传入目录存在
        FileUtils.getInstance().createDir(param.dbPath);
        param.nMaxEnergyMileage = 50; // 能源消耗保存最大公里数单位(KM)
        param.nTopArrivedMaxCnt = 8; // 常去地点列表最大个数, 也决定了获取常去地点接口返回的最大数据量
        forecastService.addObserver(this);
        int res = forecastService.init(param);
        Logger.d("initService res = " + res);
    }

    @Override
    public void registerCallBack(String key, ForecastAdapterCallback callBack) {
        callBacks.add(callBack);
    }

    /**
     * 添加常去地点
     * @param info 添加的常去地点
     * @return
     */
    @Override
    public int addLocalArrivedData(OftenArrivedItemInfo info) {
        Logger.d(TAG,"addLocalArrivedData info = " + GsonUtils.toJson(info));
        OftenArrivedItem oftenArrivedItem = new OftenArrivedItem();
        GsonUtils.copyBean(info, oftenArrivedItem);
        oftenArrivedItem.dateTime.date = info.getDate();
        oftenArrivedItem.dateTime.time = info.getTime();
        if (forecastService != null) {
            int ret = forecastService.addLocalArrivedData(ArrivedType.ForcastLocal, oftenArrivedItem);
            Logger.d(TAG,"addLocalArrivedData  ret = " + ret);
            return ret;
        }
        return 0;
    }

    /**
     * 根据POI名称删除常去地点
     * @param name 常去地点名称
     * @return ErrorCodeOK: 成功，
     */
    @Override
    public int deleteLocalArrivedData(String name) {
        Logger.d(TAG,"delLocalArrivedData name = " + name);
        if (forecastService != null) {
            int ret =  forecastService.delLocalArrivedData(ArrivedType.ForcastLocal, name);
            Logger.d(TAG,"delLocalArrivedData  ret = " + ret);
            return ret;
        }
        return 0;
    }

    /**
     * 获取常去地点信息Top列表
     * @return 返回数据
     */
    @Override
    public ArrayList<OftenArrivedItemInfo> getArrivedDataList() {
        if (forecastService != null) {
            ArrayList<OftenArrivedItem> dataList = forecastService.getArrivedDataList(ArrivedType.ForcastLocal);
            ArrayList<OftenArrivedItemInfo> infos = new ArrayList<>();
            for (OftenArrivedItem oftenArrivedItem : dataList) {
                OftenArrivedItemInfo itemInfo = GsonUtils.convertToT(oftenArrivedItem, OftenArrivedItemInfo.class);
                infos.add(itemInfo);
            }
            Logger.d(TAG,"getArrivedDataList  infos = " + GsonUtils.toJson(infos));
            return infos;
        }
        return null;
    }

    /**
     * 根据POI名称置顶
     * @param name 常去地点名称
     * @return 返回错误码
     */
    @Override
    public int topArrivedData(String name) {
        Logger.d(TAG,"topArrivedData  name = " + name);
        if (forecastService != null) {
            int ret = forecastService.topArrivedData(name);
            Logger.d(TAG,"topArrivedData  ret = " + ret);
            return ret;
        }
        return 0;
    }

    /**
     * 异步获取在线预测常去目的地(包含家、公司预测)
     * @param param 在线预测请求参数
     * @return 返回错误码
     */
    @Override
    public int getOnlineForecastArrivedData(ForecastArrivedDataInfo param) {
        Logger.d(TAG,"getOnlineForecastArrivedData  param = " + param.toString());
        ForcastArrivedParam forecastArrivedParam = new ForcastArrivedParam();
        GsonUtils.copyBean(param, forecastArrivedParam);
        if (forecastService != null) {
            return forecastService.getOnlineForcastArrivedData(forecastArrivedParam);
        }
        return 0;
    }

    /**
     * 实现预测数据服务观察者相关接口
     * @param result 初始化结果处理
     */
    @Override
    public void onInit(int result) {
        Logger.d(TAG,"onInit  result = " + result);
        for (ForecastAdapterCallback callBack : callBacks) {
            callBack.onInit(result);
        }
    }

    /**
     * 切换账号加载数据回调
     * @param result 数据加载结果
     */
    @Override
    public void onSetLoginInfo(int result) {
        Logger.d(TAG,"onSetLoginInfo  result = " + result);
        for (ForecastAdapterCallback callBack : callBacks) {
            callBack.onSetLoginInfo(result);
        }
    }

    /**
     * 异步获取在线预测常去地点(包含家、公司数据)
     * @param data 服务端预测数据
     */
    @Override
    public void onForcastArrivedData(ForcastArrivedData data) {
        ForecastArrivedDataInfo forecastArrivedDataInfo;
        forecastArrivedDataInfo = GsonUtils.convertToT(data, ForecastArrivedDataInfo.class);
        Logger.d(TAG,"onForecastArrivedData  forecastArrivedDataInfo = " + forecastArrivedDataInfo);
        for (ForecastAdapterCallback callBack : callBacks) {
            callBack.onForecastArrivedData(forecastArrivedDataInfo);
        }
    }

    /**
     * 服务反初始化
     */
    public void unInitForecastService() {
        if (forecastService != null) {
            if (ServiceInitStatus.ServiceInitDone != forecastService.isInit()) {
                forecastService.removeObserver(this);
                forecastService.unInit();
            }
        }
    }
}
