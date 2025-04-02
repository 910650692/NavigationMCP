package com.fy.navi.service.adapter.layer.bls.texture;

import static com.autonavi.gbl.map.layer.model.LayerIconType.LayerIconTypeBMP;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.VectorDrawable;
import android.text.TextUtils;
import android.util.ArrayMap;
import android.view.LayoutInflater;
import android.view.View;

import androidx.vectordrawable.graphics.drawable.VectorDrawableCompat;

import com.android.utils.file.FileUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.model.BizAGroupType;
import com.autonavi.gbl.layer.model.BizAreaType;
import com.autonavi.gbl.layer.model.BizCustomTypePoint;
import com.autonavi.gbl.layer.model.BizRouteType;
import com.autonavi.gbl.map.layer.model.Layer3DModel;
import com.autonavi.gbl.map.layer.model.LayerIconAnchor;
import com.autonavi.gbl.map.layer.model.LayerTexture;
import com.autonavi.gbl.util.model.BinaryStream;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.bls.impl.ILayerItemProcessor;
import com.fy.navi.service.define.layer.refix.LayerItemBase;

import java.nio.ByteBuffer;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicReference;

/**
 * 纹理管理类
 */
public class LayerTextureManager {
    protected static final String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    private static final String MARK_FROM_RES_TAG = "/";
    private static final String MARK_FROM_DRAWABLE_RES = "drawable";
    private static final String MARK_FROM_MIPMAP_RES = "mipmap";
    private static final String MARK_FROM_LAYOUT_RES = "layout";

    /**
     * 默认错误的资源id
     */
    public static int DEF_ERROR_ID = -1;
    public static int RIGHT_RES_ID = 0;

    private static final int MODEL_3D_ID_START = 0x10002;
    private static final int MODEL_3D_ID_MAX = 0x10010;
    private int m3DMarkerId = MODEL_3D_ID_START;

    private static final int MARK_ID_START = 0x30001;
    private static final int MARK_ID_MAX = 0x60000;
    private int mMarkerId = MARK_ID_START;

    private BitmapFactory.Options bitmapMarkerOption;

    private static final AtomicReference<Canvas> DEF_CANVAS = new AtomicReference<>(new Canvas());
    /**
     * 默认需要使用动态纹理的类型
     */
    private static final List<Integer> dynamicMarkerLayerTypeList = new CopyOnWriteArrayList<Integer>(
            List.of(
                    BizAGroupType.BizAGroupTypeAGroup,
                    BizCustomTypePoint.BizCustomTypePoint3,
                    BizAreaType.BizAreaTypeEndAreaParentPoint,
                    BizRouteType.BizRouteTypeViaETA,
                    BizRouteType.BizRouteTypeRestArea,
                    BizRouteType.BizRouteTypeViaRoad,
                    BizRouteType.BizRouteTypeWeather
            ));
    /**
     * 根据key缓存markid
     */
    private final ArrayMap<String, Integer> mAllMarkerId = new ArrayMap<>();

    private final ArrayMap<String, Integer> mAll3DMarkerId = new ArrayMap<>();

    /**
     * 缓存了drawable资源的identify
     */
    private final Set<Integer> mResNameIdSet = new HashSet<>();

    private LayerTextureManager() {
        bitmapMarkerOption = new BitmapFactory.Options();
        bitmapMarkerOption.inScaled = true;
    }

    public static LayerTextureManager get() {
        return DynamicTextureManagerHolder._INSTANCE;
    }

    private static final class DynamicTextureManagerHolder {
        public static final LayerTextureManager _INSTANCE = new LayerTextureManager();
    }

    public boolean isNotValid(int markerId) {
        return DEF_ERROR_ID == markerId;
    }


    public int getMarkerId(String markerIdKey) {
        Integer value = mAllMarkerId.get(markerIdKey);
        Logger.d(TAG, "纹理个数:" + mAllMarkerId.size()
                + "\nMarkerIdKey :" + markerIdKey
                + "\n当前 索引 mMarkerId :" + mMarkerId
                + "\nres id : " + value);
        if (value == null) {
            return DEF_ERROR_ID;
        }
        return value.intValue();
    }

    public int addMarkerId(String markerIdKey, int resID) {
        mAllMarkerId.put(markerIdKey, resID);
        mMarkerId = resID;
        Logger.d(TAG, "纹理个数:" + mAll3DMarkerId.size()
                + "\nMarkerIdKey :" + markerIdKey
                + "\n当前 索引 mMarkerId id : " + mMarkerId
                + "\nresID id : " + resID);
        return getMarkerId(markerIdKey);
    }

    public <D extends LayerItemBase> LayerTexture createLayerTexture(Context context, String markerId, String markerInfo, boolean isFocus, ILayerItemProcessor<D> processor, D data) {
        String[] tag = markerId.split(MARK_FROM_RES_TAG);
        int resID = RIGHT_RES_ID;
        if (tag.length == 2) {
            resID = context.getResources().getIdentifier(tag[1], tag[0], context.getPackageName());
        } else if (tag.length == 1) {
            resID = context.getResources().getIdentifier(tag[0], MARK_FROM_DRAWABLE_RES, context.getPackageName());
            if (resID <= RIGHT_RES_ID) {
                resID = context.getResources().getIdentifier(tag[0], MARK_FROM_MIPMAP_RES, context.getPackageName());
                if (resID <= RIGHT_RES_ID) {
                    resID = context.getResources().getIdentifier(tag[0], MARK_FROM_LAYOUT_RES, context.getPackageName());
                }
            }
        }
        if (resID <= RIGHT_RES_ID) {
            Logger.e(TAG, "markerId 资源不存在，请检查是否存在: " + markerId);
            return null;
        }
        Drawable drawable = null;
        View rootView = null;
        try {
            drawable = context.getResources().getDrawable(resID, null);
        } catch (Exception exception) {
            Logger.e(TAG, "markerId 非图片资源");
        } finally {
            try {
                rootView = LayoutInflater.from(context).inflate(resID, null, false);

            } catch (Exception exception1) {
                Logger.e(TAG, "markerId 非布局资源");
            } finally {
                if (drawable == null && rootView == null) {
                    Logger.e(TAG, "markerId 资源创建失败，请检查是否存在: " + markerId);
                    return null;
                }
                Bitmap bitmap = null;
                synchronized (LayerTextureManager.class) {
                    if (null != drawable) {
                        Logger.d(TAG, "通过 图片 创建纹理 ：" + markerId);
                        if (drawable instanceof BitmapDrawable) {
                            bitmap = BitmapFactory.decodeResource(context.getResources(), resID, bitmapMarkerOption);
                        } else if (drawable instanceof VectorDrawable || drawable instanceof VectorDrawableCompat) {
                            int requireWidth = drawable.getIntrinsicWidth() <= 0 ? 1 : drawable.getIntrinsicWidth();
                            int requireHeight = drawable.getIntrinsicHeight() <= 0 ? 1 : drawable.getIntrinsicHeight();
                            Logger.d(TAG, "图片大小 requireWidth ：" + requireWidth + " ;requireHeight :" + requireHeight);
                            bitmap = Bitmap.createBitmap(requireWidth, requireHeight, Bitmap.Config.ARGB_8888);
                            DEF_CANVAS.get().setBitmap(bitmap);
                            DEF_CANVAS.get().save();
                            // 防止 View 上面有些区域空白导致最终 Bitmap 上有些区域变黑
                            DEF_CANVAS.get().drawColor(Color.TRANSPARENT);
                            drawable.setBounds(0, 0, requireWidth, requireHeight);
                            drawable.draw(DEF_CANVAS.get());
                            DEF_CANVAS.get().restore();
                            DEF_CANVAS.get().setBitmap(null);
                        }
                    } else if (null != rootView) {
                        Logger.d(TAG, "通过 布局 创建纹理 ：" + markerId);
                        if (processor != null) {
                            if (isFocus) {
                                processor.onFocusProcess(rootView, data);
                            } else {
                                processor.onNormalProcess(rootView, data);
                            }
                        }
                        rootView.measure(0, 0);
                        rootView.layout(0, 0, rootView.getMeasuredWidth(), rootView.getMeasuredHeight());
                        int requireWidth = rootView.getMeasuredWidth() <= 0 ? 1 : rootView.getMeasuredWidth();
                        int requireHeight = rootView.getMeasuredHeight() <= 0 ? 1 : rootView.getMeasuredHeight();
                        Logger.d(TAG, "布局大小 requireWidth ：" + requireWidth + " ;requireHeight :" + requireHeight);
                        bitmap = Bitmap.createBitmap(requireWidth, requireHeight, Bitmap.Config.ARGB_8888);
                        DEF_CANVAS.get().setBitmap(bitmap);
                        DEF_CANVAS.get().save();
                        // 防止 View 上面有些区域空白导致最终 Bitmap 上有些区域变黑
                        DEF_CANVAS.get().drawColor(Color.TRANSPARENT);
                        rootView.draw(DEF_CANVAS.get());
                        DEF_CANVAS.get().restore();
                        DEF_CANVAS.get().setBitmap(null);
                    }
                }
                if (bitmap == null) {
                    Logger.e(TAG, "创建bitmap失败. markerId: " + markerId);
                    return null;
                }
                ByteBuffer dataBuffer = ByteBuffer.allocate(bitmap.getByteCount());
                bitmap.copyPixelsToBuffer(dataBuffer);
                LayerTexture layerTexture = new LayerTexture();
                layerTexture.dataBuff = new BinaryStream(dataBuffer.array());
                layerTexture.width = bitmap.getWidth();
                layerTexture.height = bitmap.getHeight();
                layerTexture.iconType = LayerIconTypeBMP;
                layerTexture.resID = createMarkerId();
                layerTexture.isPreMulAlpha = true;//纹理是否预乘透明通道,1：预乘；0：未预乘  bitmap Image are loaded with the {@link Bitmap.Config#ARGB_8888} config by default
                bitmap.recycle();
                LayerTextureMarkerInfo markerInfoBean = null;

                if (!TextUtils.isEmpty(markerInfo)) {
                    markerInfoBean = GsonUtils.fromJson(markerInfo, LayerTextureMarkerInfo.class);
                }
                if (markerInfoBean != null) {
                    layerTexture.anchorType = markerInfoBean.getAnchor();
                    layerTexture.xRatio = markerInfoBean.getX_ratio();
                    layerTexture.yRatio = markerInfoBean.getY_ratio();
                    layerTexture.isRepeat = markerInfoBean.getRepeat() == 1;
                    layerTexture.isGenMipmaps = markerInfoBean.getGen_mipmaps() == 1;
                } else {
                    layerTexture.anchorType = LayerIconAnchor.LayerIconAnchorCenter;
                    layerTexture.isRepeat = false;
                    layerTexture.xRatio = 0;
                    layerTexture.yRatio = 0;
                    layerTexture.isGenMipmaps = false;
                }
                return layerTexture;
            }
        }
    }

    private int createMarkerId() {
        int markerId = mMarkerId;
        if (mAllMarkerId.size() >= MARK_ID_MAX) {
            markerId = MARK_ID_START;
            return markerId;
        }
        Logger.d(TAG, "创建新的 markID:" + markerId);
        return markerId + 1;
    }

    public int get3DModelId(String layerName, String str3DModelId) {
        String key3DModelId = layerName + "@_" + str3DModelId;
        Integer value = mAll3DMarkerId.get(key3DModelId);
        Logger.d(TAG, "3d 纹理个数:" + mAll3DMarkerId.size());
        if (value == null) {
            return DEF_ERROR_ID;
        }
        return value;
    }


    public int add3DModelId(String layerName, String str3DModelId, int resourceID) {
        String key3DModelId = layerName + "@_" + str3DModelId;
        mAll3DMarkerId.put(key3DModelId, resourceID);
        m3DMarkerId = resourceID;
        Logger.d(TAG, "3d 纹理个数:" + mAll3DMarkerId.size() + " ; 当前3d纹理 id :" + m3DMarkerId);
        return get3DModelId(layerName, str3DModelId);
    }

    /**
     * 构建车标建模对象.
     *
     * @param str3DModelId
     * @return 建模对象
     */
    public Layer3DModel createLayer3DModel(String str3DModelId) {
        byte[] buffer = FileUtils.getInstance().getAssetFileContent(str3DModelId);
        Layer3DModel modelParam = new Layer3DModel();
        modelParam.resourceID = create3DMarkerId();
        modelParam.dataBuff = new BinaryStream(buffer);
        Logger.d(TAG, "创建3d模型 ：" + str3DModelId + "; resourceID : " + modelParam.resourceID);
        return modelParam;
    }

    /**
     * brief 获取动态纹理markerId
     * note 该id范围为[0x30001,0x60000] 超过范围会自动复位
     */
    private int create3DMarkerId() {
        int markerId = m3DMarkerId;
        if (mAll3DMarkerId.size() >= MODEL_3D_ID_MAX) {
            markerId = MODEL_3D_ID_START;
            return markerId;
        }
        return (markerId + 1);
    }
}
