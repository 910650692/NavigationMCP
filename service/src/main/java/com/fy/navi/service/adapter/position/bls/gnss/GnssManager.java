package com.fy.navi.service.adapter.position.bls.gnss;

import android.Manifest;
import android.annotation.SuppressLint;
import android.content.Context;
import android.content.pm.PackageManager;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Bundle;
import android.os.Handler;

import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;

import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.LooperType;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.position.bls.comm.GpsStatusChecker;
import com.fy.navi.service.adapter.position.bls.comm.LocationUtil;
import com.fy.navi.service.adapter.position.bls.listener.ILocationListener;
import com.fy.navi.service.adapter.position.bls.listener.IUsedSatelliteNumCallback;
import com.fy.navi.service.define.position.LocMode;


/***GNSS数据管理类***/
public class GnssManager implements LocationListener, IUsedSatelliteNumCallback, GpsStatusChecker.OnTimeOutCallback {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    protected final LocationManager mLocationManager;
    protected GSVInstrument mGSVInstrument;
    private final ILocationListener mLocationListener;
    protected final Context mContext;
    protected boolean mIsInited = false;
    protected Handler mGnssHandler;
    protected Handler mGsvHandler;
    private static int mUsedSatellite = 9;
    private LocMode mLocMode;
    private GpsStatusChecker mGpsStatusChecker;

    public GnssManager(Context context, LocationManager locationManager, ILocationListener locationListener, LocMode locMode) {
        mContext = context;
        mLocMode = locMode;
        mGnssHandler = new Handler(ThreadManager.getInstance().getLooper(LooperType.GNSS));
        mGsvHandler = new Handler(ThreadManager.getInstance().getLooper(LooperType.GSV));
        mLocationListener = locationListener;
        mLocationManager = locationManager;
        mGSVInstrument = new GSVInstrument(locationListener, this);
    }

    @SuppressLint("MissingPermission")
    public void init() {
        try {
            Logger.i(TAG, "mIsInited " + mIsInited + ",mLocMode：" + mLocMode);
            if (isPermissionsGranted()) {
                Logger.e(TAG, "Location permissions are not granted");
                return;
            }
            if (!mIsInited) {
                String provider = selectProvider();
                Logger.e(TAG, "provider " + provider);
                if (null != provider) {
                    mIsInited = true;
                    boolean result = mLocationManager.registerGnssStatusCallback(mGSVInstrument, mGsvHandler);
                    mLocationManager.requestLocationUpdates(provider,
                            1000L, // 更新间隔时间（毫秒）
                            0.0F,            // 最小距离变化（米）
                            this,
                            mGnssHandler.getLooper());
                    Logger.i(TAG, " init registerGnssStatus：" + result);
                } else {
                    Logger.e(TAG, "No location providers enabled");
                }
                if (DeviceUtils.isCar(mContext)) {
                    initGpsStatusChecker();
                }
            }
        } catch (Exception e) {
            Logger.e(TAG, "Error starting location updates", e);
        }
    }

    @SuppressLint("MissingPermission")
    public void retryRequestLocationUpdates() {
        Logger.i(TAG, " retryRequestLocationUpdates isInit：" + mIsInited);
        try {
            if (mGpsStatusChecker != null) {
                mGpsStatusChecker.clearCount();
            }
            if (DeviceUtils.isCar(mContext)) {
                if (isPermissionsGranted()) {
                    Logger.e(TAG, "Location permissions are not granted");
                    return;
                }
                if (mIsInited) {
                    mLocationManager.removeUpdates(this);
                    mLocationManager.requestLocationUpdates(selectProvider(),
                            1000L, // 更新间隔时间（毫秒）
                            0.0F,            // 最小距离变化（米）
                            this,
                            mGnssHandler.getLooper());
                }
            }
        } catch (Exception e) {
            Logger.e(TAG, "retryRequestLocationUpdates e：" + e.toString());
        }
    }

    private boolean isPermissionsGranted() {
        return ActivityCompat.checkSelfPermission(AppCache.getInstance().getMContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED
                && ActivityCompat.checkSelfPermission(AppCache.getInstance().getMContext(), Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED;
    }

    private String selectProvider() {
        if (mLocMode == LocMode.GNSS) {
            return LocationManager.NETWORK_PROVIDER;
        }
        return LocationManager.GPS_PROVIDER;
    }

    public void unInit() {
        if (mIsInited) {
            mIsInited = false;
            Logger.i(TAG, " unInit ");
            mLocationManager.removeUpdates(this);
            mLocationManager.unregisterGnssStatusCallback(mGSVInstrument);
            uninitGpsStatusChecker();
        }
    }

    private void initGpsStatusChecker() {
        mGpsStatusChecker = new GpsStatusChecker();
        mGpsStatusChecker.setTimeOutListener(this);
        mGpsStatusChecker.startGpsStatusChecker();
    }

    private void uninitGpsStatusChecker() {
        if (mGpsStatusChecker != null) {
            mGpsStatusChecker.stopGpsStatusChecker();
            mGpsStatusChecker.setTimeOutListener(null);
            mGpsStatusChecker = null;
        }
    }

    @Override
    public void onLocationChanged(@NonNull Location location) {
        if (DeviceUtils.isCar(mContext)) {
            // 输出位置信息
            final StringBuilder sb = new StringBuilder();
            sb.append("当前位置信息:")
                    .append(",精度:").append(location.getLongitude())
                    .append(",纬度:").append(location.getLatitude())
                    .append(",高度:").append(location.getAltitude())
                    .append(",速度:").append(location.getSpeed())
                    .append(",方向:").append(location.getBearing())
                    .append(",定位精度:").append(location.getAccuracy());
            Logger.d(TAG, "原生 onLocationChanged ：" + sb.toString());
        }
        if (mGpsStatusChecker != null) {
            mGpsStatusChecker.clearCount();
        }
        mLocationListener.onLocationChanged(LocationUtil.getLocGnssByLocation(location, mUsedSatellite));
    }

    @Override
    public void onStatusChanged(String provider, int status, Bundle extras) {
        Logger.d(TAG, "provider :" + provider + "status : " + status);
    }

    @Override
    public void onProviderEnabled(String provider) {

    }

    @Override
    public void onProviderDisabled(String provider) {

    }

    @Override
    public void onSatelliteNum(int num) {
        mUsedSatellite = num;
        if (mLocationListener != null) {
            mLocationListener.onSatelliteNum(num);
        }
    }

    @Override
    public void onGpsCheckTimeOut() {
        if (mLocationListener != null) {
            mLocationListener.onGpsCheckTimeOut();
        }
        retryRequestLocationUpdates();
    }
}
