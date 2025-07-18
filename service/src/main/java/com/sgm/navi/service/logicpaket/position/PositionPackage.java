package com.sgm.navi.service.logicpaket.position;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.position.IPositionAdapterCallback;
import com.sgm.navi.service.adapter.position.PositionAdapter;
import com.sgm.navi.service.adapter.position.PositionConstant;
import com.sgm.navi.service.adapter.signal.SignalAdapter;
import com.sgm.navi.service.adapter.signal.SignalAdapterCallback;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.position.DrBean;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.position.LocMMInfo;
import com.sgm.navi.service.define.position.LocParallelInfoEntity;
import com.sgm.navi.service.define.setting.SettingConstant;
import com.sgm.navi.service.define.user.usertrack.GpsTrackPointBean;
import com.sgm.navi.service.greendao.CommonManager;

import java.math.BigInteger;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;


public class PositionPackage implements IPositionAdapterCallback, SignalAdapterCallback {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private PositionAdapter mPositionAdapter = null;
    private SignalAdapter mSignalAdapter;
    private final List<IPositionPackageCallback> mIPositionCallback = new CopyOnWriteArrayList<>();
    public GeoPoint currentGeo;
    private AtomicBoolean atomicBoolean = new AtomicBoolean(false);
    private GpsTrackPointBean mGpsTrackPointBean;
    private boolean isOpenLocLog = false;

    public static PositionPackage getInstance() {
        return Helper.POSITIONING_PACKAGE;
    }

    private PositionPackage() {
    }

    public boolean init() {
        mPositionAdapter = PositionAdapter.getInstance();
        mPositionAdapter.registerCallback(this);
        mSignalAdapter = SignalAdapter.getInstance();
        Logger.i(TAG, "init：" + atomicBoolean.get());
        boolean initResult = false;
        if (!atomicBoolean.get()) {
            atomicBoolean.set(true);
            initResult  = mPositionAdapter.init();
            mSignalAdapter.registerCallback("PositionPackage", this);
            currentGeo = new GeoPoint(mPositionAdapter.getLastCarLocation().getLongitude(), mPositionAdapter.getLastCarLocation().getLatitude());
        }
        return initResult;
    }

    public synchronized void registerCallBack(IPositionPackageCallback callback) {
        if (callback != null && !mIPositionCallback.contains(callback)) {
            mIPositionCallback.add(callback);
        }
    }

    public synchronized void unregisterCallBack(IPositionPackageCallback callback) {
        if (callback != null) {
            mIPositionCallback.remove(callback);
        }
    }

    public void unInitPositionService() {
        mPositionAdapter.unInitPositionService();
        atomicBoolean.set(false);
    }

    public LocInfoBean getLastCarLocation() {
        if (mPositionAdapter == null) {
            Logger.e(TAG, "PositionAdapter is null, cannot get last car location.");
            LocInfoBean locInfoBean = new LocInfoBean();
            locInfoBean.setLongitude(SettingConstant.DEFAULT_LON_BJ);
            locInfoBean.setLatitude(SettingConstant.DEFAULT_LAT_BJ);
            locInfoBean.setAltitude(SettingConstant.DEFAULT_ALT_BJ);
            return locInfoBean;
        }
        return mPositionAdapter.getLastCarLocation();
    }

    public void startPosition() {
        mPositionAdapter.startPosition();
    }

    public void stopPosition() {
        mPositionAdapter.stopPosition();
    }

    /**切换主辅路、高架
     * @param switchRoadType 0:切换到辅路 1:切换到主路 2:切换到高架下 3:切换到高架上 -1:默认值，表示不切换
     * @param roadId 通常从onParallelRoadUpdate回调中获取  在线算路时，可以传入0 离线算路时，必须传入正确的道路ID
     * */
    public void switchParallelRoad(int switchRoadType, BigInteger roadId) {
        Logger.i(TAG, "平行路切换switchParallelRoad switchRoadType: " + switchRoadType + " roadId: " + roadId);
        mPositionAdapter.switchParallelRoad(switchRoadType, roadId);
    }

    @Override
    public void onLocationInfo(LocInfoBean locationInfo) {
        Logger.d(TAG, "onLocationInfo: " + locationInfo.toString());
        GeoPoint point = new GeoPoint();
        point.setLon(locationInfo.getLongitude());
        point.setLat(locationInfo.getLatitude());
        currentGeo = point;
        if (!ConvertUtils.isEmpty(mIPositionCallback)) {
            for (IPositionPackageCallback callback : mIPositionCallback) {
                callback.onLocationInfo(locationInfo);
            }
        }
    }

    public void saveLocStorage() {
        this.mPositionAdapter.saveLocStorage();
    }

    /**
     * 平行路切换完成回调
     */
    @Override
    public void onSwitchParallelRoadFinished() {
        if (!ConvertUtils.isEmpty(mIPositionCallback)) {
            for (IPositionPackageCallback callback : mIPositionCallback) {
                callback.onSwitchParallelRoadFinished();
            }
        }
    }

    @Override
    public void onParallelRoadUpdate(LocParallelInfoEntity entity) {
        if (!ConvertUtils.isEmpty(mIPositionCallback)) {
            for (IPositionPackageCallback callback : mIPositionCallback) {
                callback.onParallelRoadUpdate(entity);
            }
        }
    }

    @Override
    public void onGpsSatellitesChanged(boolean isLocSuccess) {
        if (!ConvertUtils.isEmpty(mIPositionCallback)) {
            for (IPositionPackageCallback callback : mIPositionCallback) {
                callback.onGpsSatellitesChanged(isLocSuccess);
            }
        }
    }

    @Override
    public void onGpsTrackPoint(GpsTrackPointBean gpsTrackPointBean) {
        Logger.i(TAG, "onGpsTrackPoint: " + GsonUtils.toJson(gpsTrackPointBean));
        mGpsTrackPointBean = gpsTrackPointBean;
    }


    /**
     * 获取GpsTrackPointBean
     *
     * @return GpsTrackPointBean
     */
    public GpsTrackPointBean getGpsTrackPointBean() {
        return mGpsTrackPointBean;
    }

    @Override
    public void onLocAnalysisResult(@PositionConstant.DRDebugEvent int infoType, String info) {
        for (IPositionPackageCallback callback : mIPositionCallback) {
            callback.onLocAnalysisResult(infoType, info);
        }
    }

    @Override
    public void onSatelliteNum(int num) {
        Logger.i(TAG, "onSatelliteNum: " + num);
        for (IPositionPackageCallback callback : mIPositionCallback) {
            callback.onSatelliteNum(num);
        }
    }

    /**
     * WGS84坐标转换为GCJ02坐标.
     *
     * @param geoPoint GeoPoint.
     * @return 包含经纬度的GCJ02坐标.
     */
    public GeoPoint wgs84ToGcj02(GeoPoint geoPoint) {
        return mPositionAdapter.wgs84ToGcj02(geoPoint);
    }

    @Override
    public void onSpeedChanged(float speed) {
        mPositionAdapter.onSpeedChanged(speed);
    }

    /**
     * 系统状态
     * @param state 系统状态
     * OFF = 0
     * ACC = 1
     * RUN = 2
     * CRANK(START) = 3
     * SLEEP = 4
     *
     *acc就是on  其他应该就是off
     */
    @Override
    public void onSystemStateChanged(int state) {
        if(state == 4){
            mPositionAdapter.saveLocStorage();
            saveLastLatLng();
            Logger.d("onSystemStateChanged1",state);
        }
    }

    @Override
    public void onGearChanged(int gear) {
        mPositionAdapter.onGearChanged(gear);
    }

    /*是否开启DR模式*/
    public void setDrEnable(boolean enable) {
        mPositionAdapter.setDrEnable(enable);
    }

    /*是否开启定位日志*/
    public void locationLogSwitch(boolean isOpen) {
        if (isOpenLocLog == isOpen) {
            return;
        }
        mPositionAdapter.locationLogSwitch(isOpen);
        isOpenLocLog = isOpen;
    }

    public boolean isOpenLocLog() {
        return isOpenLocLog;
    }

    private static final class Helper {
        private static final PositionPackage POSITIONING_PACKAGE = new PositionPackage();
    }

    private void saveLastLatLng(){
        LocInfoBean locInfoBean = getLastCarLocation();
        if(locInfoBean!=null){
            Logger.d("saveLastLatLng1",locInfoBean.getLatitude()+"--"+locInfoBean.getLongitude());
            CommonManager.getInstance().insertOrReplace(AutoMapConstant.PosLastLocation.LAST_LAT,String.valueOf(locInfoBean.getLatitude()));
            CommonManager.getInstance().insertOrReplace(AutoMapConstant.PosLastLocation.LAST_LNG,String.valueOf(locInfoBean.getLongitude()));
        }
    }
}
