package com.sgm.navi.service.adapter.position.bls.gnss;

import android.Manifest;
import android.annotation.SuppressLint;
import android.content.Context;
import android.content.pm.PackageManager;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Bundle;
import android.os.Handler;
import android.os.SystemClock;

import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;

import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.LooperType;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.pos.model.LocGnss;
import com.autonavi.gbl.pos.model.LocSpeedometer;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.position.bls.comm.LocationUtil;
import com.sgm.navi.service.adapter.position.bls.listener.ILocationListener;
import com.sgm.navi.service.adapter.position.bls.listener.IUsedSatelliteNumCallback;
import com.sgm.navi.service.define.position.LocGpgsvWrapper;
import com.sgm.navi.service.define.position.LocMode;

import java.math.BigInteger;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;


/***GNSS数据管理类***/
public class GnssManager implements LocationListener, IUsedSatelliteNumCallback {
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
    private float mCarMeterSpeed = 0f;  //仪表车速 1hz 一秒一次
    private static final int TIME_OUT = 2;
    private ScheduledFuture mScheduledFuture;
    private final AtomicInteger mTimeoutCount = new AtomicInteger(0);
    private long mLastMeterTime = 0;
    private LocGnss mLocGnss;
    private LocGpgsvWrapper mLocGpgsvWrapper;

    public GnssManager(Context context, ILocationListener locationListener, LocMode locMode) {
        mContext = context;
        mLocMode = locMode;
        mGnssHandler = new Handler(ThreadManager.getInstance().getLooper(LooperType.GNSS));
        mGsvHandler = new Handler(ThreadManager.getInstance().getLooper(LooperType.GSV));
        mLocationListener = locationListener;
        mLocationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
        mGSVInstrument = new GSVInstrument(this);
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
                mIsInited = true;
                boolean result = mLocationManager.registerGnssStatusCallback(mGSVInstrument, mGsvHandler);
                registerGps();
                mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(this::startGpsReport, 0, 1000, TimeUnit.MILLISECONDS);
                Logger.i(TAG, " init registerGnssStatus：" + result);
            }
        } catch (Exception e) {
            Logger.e(TAG, "Error starting location updates", e);
        }
    }

    @SuppressLint("MissingPermission")
    private void registerGps() {
        try {
            mLocationManager.removeUpdates(this);
            mLocationManager.requestLocationUpdates(selectProvider(),
                    1000L, // 更新间隔时间（毫秒）
                    0.0F,            // 最小距离变化（米）
                    this,
                    mGnssHandler.getLooper());
        } catch (Exception e) {
            Logger.e(TAG, "registerGps " + e.toString());
        }
    }

    @SuppressLint("MissingPermission")
    public void retryRequestLocationUpdates() {
        Logger.i(TAG, " retryRequestLocationUpdates isInit：" + mIsInited);
        try {
            mLocGnss = null;
            clearCount();
            if (DeviceUtils.isCar(mContext)) {
                if (isPermissionsGranted()) {
                    Logger.e(TAG, "Location permissions are not granted");
                    return;
                }
                if (mIsInited) {
                    registerGps();
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
            stopGpsReport();
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
        clearCount();
        mLocGnss = LocationUtil.getLocGnssByLocation(location, mUsedSatellite);
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
    public void onGSVInfo(LocGpgsvWrapper wrapper) {
        mLocGpgsvWrapper = wrapper;
    }

    private void startGpsReport() {
        Logger.i(TAG, "startGpsReport " + mTimeoutCount.get());
        reportMeterSpeed();
        if (mTimeoutCount.get() < TIME_OUT) {
            if (mLocGnss != null) {
                mLocationListener.onGpsInfo(mLocGnss);
            }
            if (mLocGpgsvWrapper != null) {
                mLocationListener.onGSVInfo(mLocGpgsvWrapper);
            }
            mTimeoutCount.incrementAndGet();
        } else {
            mLocationListener.onGpsCheckTimeOut();
            retryRequestLocationUpdates();
        }
    }

    private void stopGpsReport() {
        Logger.i(TAG, "stopGpsReport");
        if (mScheduledFuture != null) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
        }
    }

    private void reportMeterSpeed() {
        long intervalTime, tickTime;
        if (mLastMeterTime == 0) {
            mLastMeterTime = SystemClock.elapsedRealtime();
        }
        intervalTime = SystemClock.elapsedRealtime() - mLastMeterTime;
        mLastMeterTime = SystemClock.elapsedRealtime();
        tickTime = mLastMeterTime;
        LocSpeedometer locSpeedometer = new LocSpeedometer();
        locSpeedometer.value = mCarMeterSpeed;
        locSpeedometer.tickTime = new BigInteger(String.valueOf(tickTime));
        locSpeedometer.interval = (int) intervalTime;
        mLocationListener.onLocMeterInfo(locSpeedometer);
    }

    public void onMeterSpeedChanged(float speed) {
        mCarMeterSpeed = speed;
    }

    public void clearCount() {
        Logger.i(TAG, "clearCount");
        mTimeoutCount.set(0);
    }
}
