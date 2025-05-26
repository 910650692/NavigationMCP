package com.fy.navi.service.adapter.layer.bls.style;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.observer.PrepareLayerParamInner;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.CustomTextureParam;
import com.autonavi.gbl.map.layer.model.ItemStyleInfo;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.layer.refix.LayerItemData;

public class BaseStyleAdapter extends PrepareLayerParamInner {

    protected String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    public BaseStyleAdapter(int engineID) {
        super(engineID);
    }

    /**
     * 根据item的类型提供对应的item的json文件名
     *
     * @param item
     * @return
     */
    public String provideLayerItemStyleJson(LayerItem item) {
        return null;
    }

    /**
     * 是否需要重新组织json字符串
     *
     * @param item
     * @return
     */
    public boolean isNeedRefreshJsonValue(LayerItem item) {
        return false;
    }

    /***
     * 重新刷新json字符串
     * @param item
     * @param oldJson
     * @return
     */
    public String refreshOldJsonValue(LayerItem item, String oldJson) {
        return oldJson;
    }

    /**
     * 提供更新布局类型的item的数据源
     *
     * @param item
     * @return
     */
    public LayerItemData provideLayerItemData(LayerItem item) {
        return null;
    }

    /**
     * 根据item提供处理布局样式的处理器
     *
     * @param item
     * @return
     */
    public IUpdateBitmapViewProcessor provideUpdateBitmapViewProcessor(LayerItem item) {
        return null;
    }

    @Override
    public boolean updateCardContent(BaseLayer layer, LayerItem item, ItemStyleInfo styleInfo, CustomTextureParam customTextureParam) {
        boolean result = super.updateCardContent(layer, item, styleInfo, customTextureParam);
        Logger.e(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible() + ";result =" + result);
        return result;
    }
}
