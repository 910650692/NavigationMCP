package com.sgm.navi.service.adapter.layer.bls.texture;

import static com.autonavi.gbl.map.layer.model.LayerIconType.LayerIconTypeBMP;

import android.content.Context;
import android.graphics.Bitmap;

import com.android.utils.ConvertUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.SpeedCarLayerItem;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.ItemStyleInfo;
import com.autonavi.gbl.map.layer.model.Layer3DModel;
import com.autonavi.gbl.map.layer.model.LayerIconAnchor;
import com.autonavi.gbl.map.layer.model.LayerIconType;
import com.autonavi.gbl.map.layer.model.LayerTexture;
import com.autonavi.gbl.util.model.BinaryStream;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.layer.bls.style.BaseStyleAdapter;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.CrossImageEntity;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 纹理管理类
 */
public final class TexturePoolManager {

    private TexturePoolManager() {
    }

    public static TexturePoolManager get() {
        return Holder._INSTANCE;
    }

    private static final class Holder {
        public static final TexturePoolManager _INSTANCE = new TexturePoolManager();
    }

    protected String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    /**
     * 默认错误的资源id
     */
    public static int DEF_ERROR_ID = 0;

    private static final int MARK_ID_CAR_SPEED = 0x20000;

    private static final int DYN_MARK_ID_START = 0x30000;
    private static final int DYN_MARK_ID_MAX = 0x301F4;
    private int dynMarkerId = DYN_MARK_ID_START;

    private static final int MARK_ID_ARROW = 0x10000;
    private static final int MARK_ID_3D_CAR = 0x10001;

    //用于存储MarkerId key:layer.getName() + item.getBusinessType() + item.getID()
    private final ConcurrentHashMap<String, Integer> textureMap = new ConcurrentHashMap<>();

    public void add(String key, Integer value) {
        textureMap.put(key, value);
    }

    public List<String> getKeys() {
        return new ArrayList<>(textureMap.keySet());
    }

    public boolean containsKey(String key) {
        if (ConvertUtils.isEmpty(key)) {
            return false;
        }
        return textureMap.containsKey(key);
    }
    public int getValueAsInt(String key) {
        if (ConvertUtils.isEmpty(key)) {
            return DEF_ERROR_ID;
        }
        Integer i = textureMap.get(key);
        return i == null ? DEF_ERROR_ID : i;
    }

    public void removeKey(String key) {
        synchronized (textureMap) {
            if (!ConvertUtils.isEmpty(key)) {
                textureMap.remove(key);
            }
        }
    }

    public List<Integer> removeKeys(String name) {
        List<Integer> ids = new ArrayList<>();
        if (ConvertUtils.isEmpty(name)) {
            return ids;
        }
        List<String> keys = new ArrayList<>();
        for (String key : textureMap.keySet()) {
            if (key.startsWith(name)) {
                ids.add(textureMap.get(key));
                keys.add(key);
            }
        }
        for (String key : keys){
            textureMap.remove(key);
        }
        return ids;
    }

    /**
     * 是否无效纹理
     *
     * @param markerId
     * @return
     */
    public boolean isValid(int markerId) {
        return markerId > DEF_ERROR_ID;
    }

    private int createMarkerId(LayerItem item) {
        if (item instanceof SpeedCarLayerItem) {
            return MARK_ID_CAR_SPEED;
        }
        if (dynMarkerId >= DYN_MARK_ID_MAX) {
            dynMarkerId = DYN_MARK_ID_START;
        }
        Logger.d(TAG, "创建新的 markID:" + dynMarkerId);
        return dynMarkerId++;
    }


    public synchronized LayerTexture createLayerTexture(Context context, MapType mapType, BaseLayer layer, LayerItem item, ItemStyleInfo styleInfo, BaseStyleAdapter styleAdapter) {
        LayerTexture texture = new LayerTexture();
        TextureInfo textureInfo = BitmapFactoryManager.get().createBitmap(context, styleInfo.markerId, (item.getFocus() || styleInfo.markerGroup.equals("focus_style")),item,
                styleAdapter.provideUpdateBitmapViewProcessor(item), styleAdapter.provideLayerItemData(item));
        Bitmap bitmap = textureInfo.getBitmap();
        if (bitmap == null) {
            Logger.e(TAG, mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                    + " 创建bitmap失败. markerRes :" + styleInfo.markerId);
            texture.resID = -1;
            return texture;
        }

        ByteBuffer dataBuffer = ByteBuffer.allocate(bitmap.getByteCount());
        bitmap.copyPixelsToBuffer(dataBuffer);
        texture.dataBuff = new BinaryStream(dataBuffer.array());
        texture.width = bitmap.getWidth();
        texture.height = bitmap.getHeight();
        texture.iconType = LayerIconTypeBMP;
        texture.resID = createMarkerId(item);
        bitmap.recycle();

        texture.isPreMulAlpha = true;//纹理是否预乘透明通道,1：预乘；0：未预乘  bitmap Image are loaded with the {@link Bitmap.Config#ARGB_8888} config by default
        texture.anchorType = LayerIconAnchor.LayerIconAnchorCenter;
        texture.isRepeat = false;
        texture.xRatio = 0;
        texture.yRatio = 0;
        texture.isGenMipmaps = false;
        TextureMarkerInfo markerInfoBean = TextureStylePoolManager.get().getMarkerInfo(mapType, layer, item, styleInfo.markerInfo);
        texture.anchorType = markerInfoBean.getAnchor();
        texture.xRatio = markerInfoBean.getX_ratio();
        texture.yRatio = markerInfoBean.getY_ratio();
        return texture;
    }

    /**
     * 构建3D模型.
     *
     * @param str3DModelId
     * @return 建模对象
     */
    public Layer3DModel createLayer3DModel(LayerItem item, String str3DModelId) {
        byte[] buffer = FileUtils.getInstance().getAssetFileContent(str3DModelId);
        Layer3DModel modelParam = new Layer3DModel();
        //根据业务返回不同的3D模型的 markerID
        modelParam.resourceID = MARK_ID_3D_CAR;
        modelParam.dataBuff = new BinaryStream(buffer);
        Logger.d(TAG, "创建3d模型 {：" + str3DModelId + "; resourceID : " + modelParam.resourceID + "}");
        return modelParam;
    }


    public LayerTexture getArrowRoadImage(boolean isArrow, CrossImageEntity crossInfo) {
        LayerTexture image = new LayerTexture();
        image.dataBuff = new BinaryStream(isArrow ? crossInfo.getArrowDataBuf() : crossInfo.getDataBuf());
        //栅格图箭头png
        image.iconType = isArrow ? LayerIconType.LayerIconTypePNG : LayerIconType.LayerIconTypeJPG;
        image.resID = MARK_ID_ARROW;
        image.isGenMipmaps = false;
        image.isPreMulAlpha = true;
        image.isRepeat = false;
        image.anchorType = LayerIconAnchor.LayerIconAnchorLeftTop;
        return image;
    }

}
