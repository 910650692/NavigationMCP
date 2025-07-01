package com.sgm.navi.service.adapter.user.forecast.bls;

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
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.user.forecast.ForecastAdapterCallback;
import com.sgm.navi.service.adapter.user.forecast.IForecastApi;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.user.forecast.ForecastArrivedDataInfo;
import com.sgm.navi.service.define.user.forecast.OftenArrivedItemInfo;

import java.util.ArrayList;
import java.util.List;


public class ForecastAdapterImpl implements IForecastApi, IForcastServiceObserver {
    private static final String TAG = MapDefaultFinalTag.FORECAST_SERVICE_TAG;
    private ForcastService mForecastService;
    private final List<ForecastAdapterCallback> mCallBacks = new ArrayList<>();

    public ForecastAdapterImpl() {
        mForecastService = (ForcastService) ServiceMgr.getServiceMgrInstance().
                getBLService(SingleServiceID.ForcastSingleServiceID);
        Logger.i(TAG, "lvww", mForecastService);

    }

    @Override
    public void initService() {
        Logger.d("initService start.");
        if(null == mForecastService)
            mForecastService = (ForcastService) ServiceMgr.getServiceMgrInstance().
                    getBLService(SingleServiceID.ForcastSingleServiceID);
        final ForcastInitParam param = new ForcastInitParam();
        param.stCurTime = TimeUtil.getLocalTime2(); // 当前时间 com.autonavi.gbl.util.model.DateTime
        param.dbPath = GBLCacheFilePath.FORECAST_PATH;// 预测数据库文件保存目录路径
        // 保证传入目录存在
        FileUtils.getInstance().createDir(param.dbPath);
        param.nMaxEnergyMileage = 50; // 能源消耗保存最大公里数单位(KM)
        param.nTopArrivedMaxCnt = 8; // 常去地点列表最大个数, 也决定了获取常去地点接口返回的最大数据量
        mForecastService.addObserver(this);
        final int res = mForecastService.init(param);
        Logger.d("initService res = " , res);
    }

    @Override
    public void registerCallBack(final String key, final ForecastAdapterCallback callBack) {
        mCallBacks.add(callBack);
    }

    /**
     * 添加常去地点
     * @param info 添加的常去地点
     * @return
     */
    @Override
    public int addLocalArrivedData(final OftenArrivedItemInfo info) {
        Logger.d(TAG,"addLocalArrivedData info = " , GsonUtils.toJson(info));
        final OftenArrivedItem oftenArrivedItem = new OftenArrivedItem();
        GsonUtils.copyBean(info, oftenArrivedItem);
        oftenArrivedItem.dateTime.date = info.getDate();
        oftenArrivedItem.dateTime.time = info.getTime();
        if (mForecastService != null) {
            final int ret = mForecastService.addLocalArrivedData(ArrivedType.ForcastLocal, oftenArrivedItem);
            Logger.d(TAG,"addLocalArrivedData  ret = " , ret);
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
    public int deleteLocalArrivedData(final String name) {
        Logger.d(TAG,"delLocalArrivedData name = " , name);
        if (mForecastService != null) {
            final int ret =  mForecastService.delLocalArrivedData(ArrivedType.ForcastLocal, name);
            Logger.d(TAG,"delLocalArrivedData  ret = " , ret);
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
        if (mForecastService != null) {
            final ArrayList<OftenArrivedItem> dataList = mForecastService.getArrivedDataList(ArrivedType.ForcastLocal);
            final ArrayList<OftenArrivedItemInfo> infos = new ArrayList<>();
            for (OftenArrivedItem oftenArrivedItem : dataList) {
                final OftenArrivedItemInfo itemInfo = getOftenArrivedItemInfo(oftenArrivedItem);
                infos.add(itemInfo);
            }
            Logger.d(TAG,"getArrivedDataList  infos = " , GsonUtils.toJson(infos));
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
    public int topArrivedData(final String name) {
        Logger.d(TAG,"topArrivedData  name = " + name);
        if (mForecastService != null) {
            final int ret = mForecastService.topArrivedData(name);
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
    public int getOnlineForecastArrivedData(final ForecastArrivedDataInfo param) {
        Logger.d(TAG,"getOnlineForecastArrivedData  param = " + param.toString());
        final ForcastArrivedParam forecastArrivedParam = new ForcastArrivedParam();
        forecastArrivedParam.nLevel = param.getLevel();
        forecastArrivedParam.adCode = param.getAdCode();
        forecastArrivedParam.userId = param.getUserId();
        forecastArrivedParam.userLoc.lon = param.getUserLoc().getLon();
        forecastArrivedParam.userLoc.lat = param.getUserLoc().getLat();
        if (mForecastService != null) {
            return mForecastService.getOnlineForcastArrivedData(forecastArrivedParam);
        }
        return 0;
    }

    /**
     * 实现预测数据服务观察者相关接口
     * @param result 初始化结果处理
     */
    @Override
    public void onInit(final int result) {
        Logger.d(TAG,"onInit  result = " + result);
        for (ForecastAdapterCallback callBack : mCallBacks) {
            callBack.onInit(result);
        }
    }

    /**
     * 切换账号加载数据回调
     * @param result 数据加载结果
     */
    @Override
    public void onSetLoginInfo(final int result) {
        Logger.d(TAG,"onSetLoginInfo  result = " + result);
        for (ForecastAdapterCallback callBack : mCallBacks) {
            callBack.onSetLoginInfo(result);
        }
    }

    /**
     * 异步获取在线预测常去地点(包含家、公司数据)
     * @param data 服务端预测数据
     */
    @Override
    public void onForcastArrivedData(final ForcastArrivedData data) {
        final ForecastArrivedDataInfo forecastArrivedDataInfo;
        forecastArrivedDataInfo = getForecastArrivedDataInfo(data);
        Logger.d(TAG,"onForecastArrivedData  forecastArrivedDataInfo = " + forecastArrivedDataInfo);
        for (ForecastAdapterCallback callBack : mCallBacks) {
            callBack.onForecastArrivedData(forecastArrivedDataInfo);
        }
    }

    /**
     * 服务反初始化
     */
    public void unInitForecastService() {
        if (mForecastService != null) {
            if (ServiceInitStatus.ServiceInitDone != mForecastService.isInit()) {
                mForecastService.removeObserver(this);
                mForecastService.unInit();
            }
        }
    }

    /**
     * 数据转换
     * @param oftenArrivedItem 常去地点
     * @return 常去地点信息
     */
    public OftenArrivedItemInfo getOftenArrivedItemInfo(final OftenArrivedItem oftenArrivedItem) {
        final OftenArrivedItemInfo oftenArrivedItemInfo = new OftenArrivedItemInfo();
        oftenArrivedItemInfo.setWstrPoiID(oftenArrivedItem.wstrPoiID);
        oftenArrivedItemInfo.setWstrPoiName(oftenArrivedItem.wstrPoiName);
        oftenArrivedItemInfo.setWstrPoiType(oftenArrivedItem.wstrPoiType);
        oftenArrivedItemInfo.setWstrAddress(oftenArrivedItem.wstrAddress);
        oftenArrivedItemInfo.setStDisplayCoord(new GeoPoint(oftenArrivedItem.stDisplayCoord.lon, oftenArrivedItem.stDisplayCoord.lat));
        oftenArrivedItemInfo.setStNaviCoord(new GeoPoint(oftenArrivedItem.stNaviCoord.lon, oftenArrivedItem.stNaviCoord.lat));
        oftenArrivedItemInfo.setParent(oftenArrivedItem.parent);
        oftenArrivedItemInfo.setTowardsAngle(oftenArrivedItem.towardsAngle);
        oftenArrivedItemInfo.setFloorNo(oftenArrivedItem.floorNo);
        oftenArrivedItemInfo.setChildType(oftenArrivedItem.childType);
        oftenArrivedItemInfo.setEndPoiExtension(oftenArrivedItem.endPoiExtension);
        oftenArrivedItemInfo.setTopTime(oftenArrivedItem.topTime);
        oftenArrivedItemInfo.setTimeList(oftenArrivedItem.uTimeList);
        oftenArrivedItemInfo.setDate(oftenArrivedItem.dateTime.date);
        oftenArrivedItemInfo.setTime(oftenArrivedItem.dateTime.time);
        return oftenArrivedItemInfo;
    }

    /**
     * 数据转换
     * @param param 服务端预测数据
     * @return 预测数据
     */
    public ForecastArrivedDataInfo getForecastArrivedDataInfo(final ForcastArrivedData param) {
        final ForecastArrivedDataInfo forecastArrivedDataInfo = new ForecastArrivedDataInfo();
        forecastArrivedDataInfo.setHome(getOftenArrivedItemInfo(param.home));
        forecastArrivedDataInfo.setCompany(getOftenArrivedItemInfo(param.company));
        final ArrayList<OftenArrivedItemInfo> getOftenArrivedItemInfos = new ArrayList<>();
        for (OftenArrivedItem oftenArrivedItem : param.others) {
            final OftenArrivedItemInfo oftenArrivedItemInfo = getOftenArrivedItemInfo(oftenArrivedItem);
            getOftenArrivedItemInfos.add(oftenArrivedItemInfo);
        }
        forecastArrivedDataInfo.setOthers(getOftenArrivedItemInfos);
        return forecastArrivedDataInfo;
    }

}
