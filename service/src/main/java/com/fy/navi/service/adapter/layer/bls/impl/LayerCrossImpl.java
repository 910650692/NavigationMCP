package com.fy.navi.service.adapter.layer.bls.impl;

import android.content.Context;

import com.autonavi.gbl.common.model.RectInt;
import com.autonavi.gbl.guide.model.CrossType;
import com.autonavi.gbl.guide.model.NaviInfo;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.model.LayerIconAnchor;
import com.autonavi.gbl.map.layer.model.LayerIconType;
import com.autonavi.gbl.map.layer.model.LayerTexture;
import com.autonavi.gbl.map.layer.model.RealCityTmcParam;
import com.autonavi.gbl.map.layer.model.VectorCrossViewPostureEvent;
import com.fy.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.autonavi.gbl.util.model.BinaryStream;

import java.util.ArrayList;

public class LayerCrossImpl extends BaseLayerImpl {

    public LayerCrossImpl(BizControlService bizService, MapView mapView, Context context) {
        super(bizService, mapView, context);
    }

    private int getCrossType(int type) {
        return switch (type) {
            case 1 -> CrossType.CrossTypeGrid;
            case 3 -> CrossType.CrossTypeVector;
            case 4 -> CrossType.CrossType3D;
            default -> CrossType.AUTO_UNKNOWN_ERROR;
        };
    }

    /* 根据放大路口图层类型更新样式 */
    public boolean updateCrossStyle(@CrossType.CrossType1 int crossType) {
        getLayerRoadCrossControl().updateStyle(crossType);
        return true;
    }

    /* 根据放大路口类型进行显示隐藏控制 */
    public boolean setCrossVisible(@CrossType.CrossType1 int type, boolean bVisible) {
        getLayerRoadCrossControl().setVisible(type, bVisible);
        return true;
    }

    /* 设置栅格图图片数据 */
    public boolean setRasterImageData(LayerItemCrossEntity crossEntity) {
        CrossImageEntity crossImageEntity = crossEntity.getCrossImageEntity();
        int type = crossImageEntity.getType();
        if (type == 3 || type == 4) {   //矢量图或者三维图
            getLayerRoadCrossControl().updateCross(crossImageEntity.getDataBuf(), getCrossType(type));
        } else if (type == 1) {  //栅格图
            LayerTexture arrowImge = new LayerTexture();
            LayerTexture roadImage = new LayerTexture();
            arrowImge.dataBuff = new BinaryStream(crossImageEntity.getArrowDataBuf());
            arrowImge.iconType = LayerIconType.LayerIconTypePNG; // 栅格图箭头为png
//            arrowImge.resID = info.crossImageID.intValue();
            arrowImge.isGenMipmaps = false;
            arrowImge.isPreMulAlpha = true;
            arrowImge.isRepeat = false;
            arrowImge.anchorType = LayerIconAnchor.LayerIconAnchorLeftTop;  // 栅格图的锚点一定要设置为左上角，避免偏移

            roadImage.dataBuff = new BinaryStream(crossImageEntity.getDataBuf());
            roadImage.iconType = LayerIconType.LayerIconTypeJPG;  // 栅格图背景图为jpg
//            roadImage.resID = info.crossImageID.intValue();
            roadImage.isGenMipmaps = false;
            roadImage.isPreMulAlpha = true;
            roadImage.isRepeat = false;
            roadImage.anchorType = LayerIconAnchor.LayerIconAnchorLeftTop;  // 栅格图的锚点一定要设置为左上角，避免偏移

            getLayerRoadCrossControl().setRasterImageData(arrowImge, roadImage);
        }
        getLayerRoadCrossControl().setVisible(getCrossType(type), true);
        return true;
    }

    /* 根据放大路口类型填充数据 */
    public boolean updateCross(LayerItemCrossEntity crossEntity) {
        CrossImageEntity crossImageEntity = crossEntity.getCrossImageEntity();
        int type = crossImageEntity.getType();
        getLayerRoadCrossControl().updateCross(crossImageEntity.getDataBuf(), getCrossType(type));
        return true;
    }

    /* 根据放大路口类型隐藏对应的路口大图 */
    public boolean hideCross(@CrossType.CrossType1 int type) {
        getLayerRoadCrossControl().hideCross(type);
        return true;
    }

    /* 设置导航车首上还是北上模式 */
    public boolean set3DCrossCarMode(boolean isCarUp) {
        getLayerRoadCrossControl().set3DCrossCarMode(isCarUp);
        return true;
    }

    /* 设置3D飞线的路况信息 */
    public boolean setFlyTmc(byte[] buffer, ArrayList<RealCityTmcParam> param) {
        getLayerRoadCrossControl().setFlyTmc(buffer, param);
        return true;
    }

    /* 更新3D精品大图引导信息 */
    public boolean updateNaviInfo(NaviInfo naviInfo) {
        return true;
    }

    /* 设置路口栅格图信息
     *1、设置路口大图信息，使用自带近接/混淆矢量大图显隐策略，如果没有设置数据，用户可自定义策略并调用SetViewPostureEvent触发功能
     *2、本接口暂时只对混淆\近接路口生效，当设置路口大图信息，用户调用SetViewPostureEvent无效（内部策略自动调用
     */
    public boolean setCrossImageInfo(@CrossType.CrossType1 int type, boolean useCustom) {
//        getLayerRoadCrossControl().setCrossImageInfo()
        return true;
    }

    /* 设置近接/混淆矢量大图的姿态事件, 目前只有type = CrossTypeVector才有实现才有实现 */
    public boolean setViewPostureEvent(CrossType type, VectorCrossViewPostureEvent postureEvent) {
//        getLayerRoadCrossControl().setViewPostureEvent(type, postureEvent);
        return true;
    }

    /* 设置放大路口显示区域 */
    public boolean setRoadCrossRect(@CrossType.CrossType1 int crossType, RectInt viewRect) {
        getLayerRoadCrossControl().setRoadCrossRect(crossType, viewRect);
        return true;
    }

}
