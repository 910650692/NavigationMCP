package com.sgm.navi.service.adapter.position.bls.gnss;

import android.location.GnssStatus;
import android.os.SystemClock;

import com.android.utils.log.Logger;
import com.autonavi.gbl.pos.model.LocGpgsv;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.position.bls.listener.IUsedSatelliteNumCallback;
import com.sgm.navi.service.define.position.LocGpgsvWrapper;

import java.math.BigInteger;

/***卫星星历获取***/
public class GSVInstrument extends GnssStatus.Callback {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private IUsedSatelliteNumCallback mCallback;
    /**
     * 星历卫星数据最大值
     */
    public static final int MAX_GPS_SATELLITE_NUM = 16;

    public GSVInstrument(IUsedSatelliteNumCallback callback) {
        mCallback = callback;
    }

    @Override
    public void onSatelliteStatusChanged(GnssStatus status) {
        Logger.d(TAG, "SatelliteStatus ");
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
            // 可见卫星的唯一标识符 GPS：1~32 SBAS：33~64
            prn = gnssStatus.getSvid(i);
            //usedInFix是判断卫星是否可用，但GSV需要的是所有可见的卫星星历信息，故无需增加该判断
            if (prn > 0 && prn <= 64) {
                tempPrn[count] = prn;
                // 卫星仰角，仰角越大，信号越好
                tempElevation[count] = (int) gnssStatus.getElevationDegrees(i);
                //获取的是第 i 颗卫星的方位角（Azimuth），单位为度（°），范围通常是 0~360。方位角表示卫星相对于观测点正北方向的夹角，用于描述卫星在天空中的水平位置
                tempAzimuth[count] = (int) gnssStatus.getAzimuthDegrees(i);
                // 获取的是第 i 颗卫星的载噪比（C/N0，Carrier-to-Noise density ratio），单位为 dB-Hz。它表示信号强度与噪声密度的比值，数值越大，说明该卫星信号越好。常用于评估卫星信号质量。
                snr = (int) gnssStatus.getCn0DbHz(i);
                tempSnr[count] = snr;
                Logger.d(TAG, "setGSVData: prn:", prn, ",snr:", snr);
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
            mCallback.onGSVInfo(wrapper);
//            Logger.d(TAG, "setGSVData:");
        } else {
            Logger.e(TAG, "n：" + n + ",realtime：" + realtime);
        }
    }
}
