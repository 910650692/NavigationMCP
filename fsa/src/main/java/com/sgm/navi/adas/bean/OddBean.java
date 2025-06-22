package com.sgm.navi.adas.bean;

import com.android.utils.log.Logger;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteOdd;

import java.util.ArrayList;

/**
 * OddBean
 * Map provider向MFF返回的matching response的接口和数据结构
 */
public class OddBean {
    private static final String TAG = "OddBean";
    /**
     * matching异常状态CODE
     * 0 正常
     * 1 车端数据版本错误无法返回云端算路结果(version错误)
     * 2 车端数据所属项目不支持云端算路（catalog错误）
     * 3 服务鉴权失败（http401、403）
     * 4 输入mode错误，不等于1或2，云端无需响应
     * 5 请求体格式异常解析失败
     * 6 服务内部异常，算路结果为空
     * 7 必填参数缺失
     *
     * 12 云端算路返回结果为空或不合法
     * 13 PNP模式下，本地算路失败
     * 14 本地数据获取不到合法的catalog或verison
     * 501 “取消路线”成功
     * 502 “取消路线”失败
     * 503 冷加载实时拉取HD Map成功
     * 504 冷加载实时拉取HD Map失败
     *
     * 99 状态未知
     */
    private int error_code;
    /**
     * 异常CODE对应的信息，正常为空
     */
    private String error_message;
    /**
     * 云端服务返回的结构体
     */
    private OddResponse response;

    public int getError_code() {
        return error_code;
    }

    public void setError_code(int error_code) {
        this.error_code = error_code;
    }

    public String getError_message() {
        return error_message;
    }

    public void setError_message(String error_message) {
        this.error_message = error_message;
    }

    public OddResponse getResponse() {
        return response;
    }

    public void setResponse(OddResponse response) {
        this.response = response;
    }

    public ArrayList<LayerItemRouteOdd> toLayerItemRouteOddList() {
        if (this.getError_code() == 0) {
            OddResponse response = this.getResponse();
            if (response == null) {
                Logger.e(TAG, "response null");
                return null;
            }
            ArrayList<SwitchSegment> switchSegments = response.getSwitch_segments();
            if (switchSegments == null) {
                Logger.e(TAG, "switchSegments null");
                return null;
            }
            ArrayList<LayerItemRouteOdd> oddInfoList = new ArrayList<>();
            for (SwitchSegment switchSegment : switchSegments) {
                if (switchSegment == null) {
                    continue;
                }
                /*
                0 UNMATCH
                9 sd unp
                10 sd hnp
                11 sd odd close
                 */
                int mode = switchSegment.getMode();
                if (mode != 9 && mode != 10) {
                    continue;
                }
                LayerItemRouteOdd layerItemRouteOdd = new LayerItemRouteOdd();
                try {
                    ArrayList<Coord> coords = switchSegment.getCoords();
                    Coord startCoord = coords.get(0);
                    Coord endCoord = coords.get(coords.size() - 1);
                    layerItemRouteOdd.addGeoPoint(new GeoPoint(startCoord.getX(), startCoord.getY()));
                    layerItemRouteOdd.addGeoPoint(new GeoPoint(endCoord.getX(), endCoord.getY()));
                } catch (Exception e) {
                    Logger.e(TAG, "error " + e.getMessage());
                }
                oddInfoList.add(layerItemRouteOdd);
            }
            return oddInfoList;
        } else {
            Logger.e(TAG, "ErrorCode = " + getError_code() + "--" + getError_message());
            return null;
        }
    }

    public long getPathId() {
        if (this.getError_code() == 0) {
            OddResponse response = this.getResponse();
            if (response == null) {
                Logger.e(TAG, "response null");
                return -1;
            }
            long pathId = response.getPath_id();
            Logger.e(TAG, "pathId = " + pathId);
            return pathId;
        } else {
            Logger.e(TAG, "ErrorCode = " + getError_code() + "--" + getError_message());
            return -1;
        }
    }
}
