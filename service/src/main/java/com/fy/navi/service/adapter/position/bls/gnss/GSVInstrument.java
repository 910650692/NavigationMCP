package com.fy.navi.service.adapter.position.bls.gnss;

import android.location.GnssStatus;
import android.os.SystemClock;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.autonavi.gbl.pos.model.LocGpgsv;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.position.bls.listener.ILocationListener;
import com.fy.navi.service.adapter.position.bls.listener.IUsedSatelliteNumCallback;
import com.fy.navi.service.define.position.LocGpgsvWrapper;

import java.math.BigInteger;

/***卫星星历获取***/
public class GSVInstrument extends GnssStatus.Callback {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private ILocationListener mListener;
    private IUsedSatelliteNumCallback mCallback;
    /**
     * 星历卫星数据最大值
     */
    public static final int MAX_GPS_SATELLITE_NUM = 16;

    public GSVInstrument(@NonNull ILocationListener listener, IUsedSatelliteNumCallback callback) {
        mListener = listener;
        mCallback = callback;
    }

    @Override
    public void onSatelliteStatusChanged(GnssStatus status) {
        Logger.i(TAG, "SatelliteStatus ");
        if (status == null) {
            return;
        }
        setGSVData(status);
    }

    public void setGSVData(GnssStatus gnssStatus) {
        LocGpgsvWrapper wrapper = new LocGpgsvWrapper();
        LocGpgsv locGpgsvGPS = wrapper.locGpgsv;
        int[] tempPrn = new int[MAX_GPS_SATELLITE_NUM];
        int[] tempElevation = new int[MAX_GPS_SATELLITE_NUM];
        int[] tempAzimuth = new int[MAX_GPS_SATELLITE_NUM];
        int[] tempSnr = new int[MAX_GPS_SATELLITE_NUM];
        int validCount = 0;
        int count = 0;
        int prn;
        int snr = -1;
        for (int i = 0; i < gnssStatus.getSatelliteCount() && count < MAX_GPS_SATELLITE_NUM; i++) {
            prn = gnssStatus.getSvid(i);
            //usedInFix是判断卫星是否可用，但GSV需要的是所有可见的卫星星历信息，故无需增加该判断
            if (prn > 0 && prn <= 64) {
                tempPrn[count] = prn;
                tempElevation[count] = (int) gnssStatus.getElevationDegrees(i);
                tempAzimuth[count] = (int) gnssStatus.getAzimuthDegrees(i);
                snr = (int) gnssStatus.getCn0DbHz(i);
                tempSnr[count] = snr;
                count++;
                if (snr > 0) {
                    validCount++;
                }
            }
        }

        int n = 0;
        int totalSnr = 0;
        for (int i = 0; i < count; i++) {
            if (validCount > 0 && tempSnr[i] <= 0) {
                continue;
            }
            locGpgsvGPS.prn[n] = tempPrn[i];
            locGpgsvGPS.elevation[n] = tempElevation[i];
            locGpgsvGPS.azimuth[n] = tempAzimuth[i];
            locGpgsvGPS.snr[n] = tempSnr[i];
            totalSnr += tempSnr[i];
            n++;
        }
        locGpgsvGPS.num = n;
        if (mCallback != null) {
            mCallback.onSatelliteNum(n);
        }
        long realtime = SystemClock.elapsedRealtime();
        if (n > 0 && realtime > 0) {
            // 时间戳需与GNSS信号传入的时间戳同源
            locGpgsvGPS.tickTime = BigInteger.valueOf(realtime);
            wrapper.attributes.averageSnr = totalSnr / n;
            mListener.onGSVInfo(wrapper);
        }
    }
}
