package com.fy.navi.service.adapter.layer.bls.texture;

import static com.autonavi.gbl.map.layer.model.LayerIconType.LayerIconTypeBMP;

import android.content.Context;
import android.graphics.Bitmap;
import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.SpeedCarLayerItem;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.ItemStyleInfo;
import com.autonavi.gbl.map.layer.model.Layer3DModel;
import com.autonavi.gbl.map.layer.model.LayerIconAnchor;
import com.autonavi.gbl.map.layer.model.LayerIconType;
import com.autonavi.gbl.map.layer.model.LayerTexture;
import com.autonavi.gbl.util.model.BinaryStream;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.bls.bean.MarkerInfoBean;
import com.fy.navi.service.adapter.layer.bls.style.BaseStyleAdapter;
import com.fy.navi.service.adapter.layer.bls.utils.StyleJsonAnalysisUtil;
import com.fy.navi.service.define.navi.CrossImageEntity;

import java.nio.ByteBuffer;

/**
 * 纹理管理类
 */
public final class TexturePoolManager {

    private static final String mFileStringFromAssets = com.fy.navi.service.adapter.layer.bls.utils.FileUtils.getFileStringFromAssets(AppCache.getInstance().getMApplication(), GBLCacheFilePath.BLS_ASSETS_LAYER_CUSTOM_MARKER_INFO_PATH);
    public static final StyleJsonAnalysisUtil sUtil = new StyleJsonAnalysisUtil(mFileStringFromAssets);

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
    private static final int DYN_MARK_ID_MAX = 0x60000;
    private int dynMarkerId = DYN_MARK_ID_START;

    private static final int MARK_ID_ARROW = 0x10000;
    private static final int MARK_ID_3D_CAR = 0x10001;


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


    public synchronized LayerTexture createLayerTexture(Context context, LayerItem item, ItemStyleInfo styleInfo, BaseStyleAdapter styleAdapter) {
        LayerTexture texture = new LayerTexture();
        TextureInfo textureInfo = BitmapFactoryManager.get().createBitmap(context, styleInfo.markerId, (item.getFocus() || styleInfo.markerGroup.equals("focus_style")),
                styleAdapter.provideUpdateBitmapViewProcessor(item), styleAdapter.provideLayerItemData(item));
        Bitmap bitmap = textureInfo.getBitmap();
        if (bitmap == null) {
            Logger.d(TAG, "创建bitmap失败. markerRes: " + styleInfo.markerId);
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

        Logger.d(TAG, "createLayerTexture markerInfo " + styleInfo.markerInfo);
        if (!TextUtils.isEmpty(styleInfo.markerInfo)) {
            try {
                MarkerInfoBean markerInfoFromJson = sUtil.getMarkerInfoFromJson(styleInfo.markerInfo);
                if (!ConvertUtils.isEmpty(markerInfoFromJson)) {
                    TextureMarkerInfo markerInfoBean = GsonUtils.fromJsonV2(GsonUtils.toJson(markerInfoFromJson), TextureMarkerInfo.class);
                    texture.anchorType = markerInfoBean.getAnchor();
                    texture.xRatio = markerInfoBean.getX_ratio();
                    texture.yRatio = markerInfoBean.getY_ratio();
                    Logger.e(TAG, "采用自定义 markerInfo: " + markerInfoBean.toString() + GsonUtils.toJson(markerInfoBean));
                }
            } catch (Exception exception) {
                Logger.e(TAG, "markerInfo 信息出错: " + exception.toString());
            }
        }
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
