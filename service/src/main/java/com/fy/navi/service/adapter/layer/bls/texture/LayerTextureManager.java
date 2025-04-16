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

import com.android.utils.ConvertUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.map.layer.model.Layer3DModel;
import com.autonavi.gbl.map.layer.model.LayerIconAnchor;
import com.autonavi.gbl.map.layer.model.LayerIconType;
import com.autonavi.gbl.map.layer.model.LayerTexture;
import com.autonavi.gbl.util.model.BinaryStream;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.bls.style.IUpdateBitmapViewProcessor;
import com.fy.navi.service.define.layer.refix.LayerItemData;
import com.fy.navi.service.define.navi.CrossImageEntity;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicReference;

/**
 * 纹理管理类
 */
public class LayerTextureManager {

    protected String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    private static final String MARK_FROM_RES_TAG = "/";
    private static final String MARK_FROM_DRAWABLE_RES = "drawable";
    private static final String MARK_FROM_MIPMAP_RES = "mipmap";
    private static final String MARK_FROM_LAYOUT_RES = "layout";

    private static final int CREATE_BITMAP_RETRY_COUNT = 3;
    /**
     * 默认错误的资源id
     */
    public static int DEF_ERROR_ID = 0;

    private static final int MARK_ID_START = 0x30001;
    private static final int MARK_ID_MAX = 0x60000;
    private int mMarkerId = MARK_ID_START;


    private static final AtomicReference<Canvas> DEF_CANVAS = new AtomicReference<>(new Canvas());

    /**
     * 根据key缓存markid
     */
    private final ArrayMap<String, Integer> mAllMarkerId = new ArrayMap<>();

    private LayerTextureManager() {
    }

    public static LayerTextureManager get() {
        return DynamicTextureManagerHolder._INSTANCE;
    }

    private static final class DynamicTextureManagerHolder {
        public static final LayerTextureManager _INSTANCE = new LayerTextureManager();
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


    private int createMarkerId() {
        int markerId = mMarkerId;
        if (mAllMarkerId.size() >= MARK_ID_MAX) {
            markerId = MARK_ID_START;
            Logger.e(TAG, "markID资源已经超过最大值:");
            return markerId;
        }
        Logger.d(TAG, "创建新的 markID:" + markerId);
        return markerId + 1;
    }


    public int getMarkerId(String markerIdKey) {
        Integer value = mAllMarkerId.get(markerIdKey);
        Logger.d(TAG, "当前 纹理个数:" + mAllMarkerId.size() + " ; 当前 mMarkerId :" + mMarkerId
                + "\n{ MarkerIdKey :" + markerIdKey + " ;resId: " + value + " }");
        return value.intValue();
    }

    public boolean isHasMarkerId(String markerIdKey) {
        return mAllMarkerId.containsKey(markerIdKey);
    }

    public int addMarkerId(String markerIdKey, int resID) {
        mAllMarkerId.put(markerIdKey, resID);
        mMarkerId = resID;
        Logger.d(TAG, "当前 纹理个数:" + mAllMarkerId.size() + " ; 当前 mMarkerId :" + mMarkerId
                + "\n{ MarkerIdKey :" + markerIdKey + " ;resId: " + resID + " }");
        return mMarkerId;
    }

    public synchronized void clearMarkerId(String markerIdKey) {
        ArrayList<String> markerIdKeys = new ArrayList<>();
        for (String key : mAllMarkerId.keySet()) {
            if (key.startsWith(markerIdKey)) {
                markerIdKeys.add(key);
            }
        }
        mAllMarkerId.removeAll(markerIdKeys);
        mMarkerId = mMarkerId - markerIdKeys.size();
        Logger.d(TAG, "当前 纹理个数:" + mAllMarkerId.size() + "；删除marker：" + markerIdKey + " ; 删除的纹理数量 ：" + markerIdKeys.size() + " ; 当前 mMarkerId :" + mMarkerId);
    }

    public synchronized <D extends LayerItemData> LayerTexture createLayerTexture(Context context, String markerId, String markerInfo, boolean isFocus,
                                                                                  IUpdateBitmapViewProcessor<D> processor, D data) {

        Logger.d(TAG, "创建bitmap ， markerId: " + markerId + " ;markerInfo : " + markerInfo);
        Bitmap bitmap = createBitmap(context, markerId, isFocus, processor, data);
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
        bitmap.recycle();

        layerTexture.isPreMulAlpha = true;//纹理是否预乘透明通道,1：预乘；0：未预乘  bitmap Image are loaded with the {@link Bitmap.Config#ARGB_8888} config by default
        layerTexture.anchorType = LayerIconAnchor.LayerIconAnchorCenter;
        layerTexture.isRepeat = false;
        layerTexture.xRatio = 0;
        layerTexture.yRatio = 0;
        layerTexture.isGenMipmaps = false;

        LayerTextureMarkerInfo markerInfoBean = new LayerTextureMarkerInfo();
        try {
            markerInfoBean = GsonUtils.fromJsonV2(markerInfo, LayerTextureMarkerInfo.class);
        } catch (Exception exception) {
            Logger.e(TAG, "markerInfo 信息出错: " + exception.toString());
        }
        if (!ConvertUtils.isNull(markerInfoBean)) {
            layerTexture.anchorType = markerInfoBean.getAnchor();
            layerTexture.xRatio = markerInfoBean.getX_ratio();
            layerTexture.yRatio = markerInfoBean.getY_ratio();
            Logger.e(TAG, "markerInfo 信息: " + markerInfoBean.toString());
        }
        return layerTexture;
    }


    public synchronized <D extends LayerItemData> Bitmap createBitmap(Context context, String markerId, boolean isFocus, IUpdateBitmapViewProcessor<D> processor, D data) {
        if (TextUtils.isEmpty(markerId)) {
            Logger.e(TAG, "markerId 资源未配置 : ");
            return null;
        }
        String[] tag = markerId.split(MARK_FROM_RES_TAG);
        int resID = DEF_ERROR_ID;
        if (tag.length == 2) {
            resID = context.getResources().getIdentifier(tag[1], tag[0], context.getPackageName());
        } else if (tag.length == 1) {
            resID = context.getResources().getIdentifier(tag[0], MARK_FROM_DRAWABLE_RES, context.getPackageName());
            if (!isValid(resID)) {
                resID = context.getResources().getIdentifier(tag[0], MARK_FROM_MIPMAP_RES, context.getPackageName());
                if (!isValid(resID)) {
                    resID = context.getResources().getIdentifier(tag[0], MARK_FROM_LAYOUT_RES, context.getPackageName());
                }
            }
        }
        if (!isValid(resID)) {
            Logger.e(TAG, "markerId 资源不存在，请检查是否存在: " + markerId);
            return null;
        }
        Drawable drawable = null;
        View rootView = null;
        try {
            drawable = context.getResources().getDrawable(resID, null);
        } catch (Exception exception) {
            Logger.e(TAG, markerId + " 不存在 Drawable / mipmap 文件夹中");
        } finally {
            try {
                rootView = LayoutInflater.from(context).inflate(resID, null, false);
            } catch (Exception exception) {
                Logger.e(TAG, markerId + " 不存在 Layout 文件夹中");
            } finally {
                Bitmap bitmap = null;
                if (null != drawable) {
                    Logger.e(TAG, "优先 使用 图片 创建纹理 ：" + markerId);
                    if (drawable instanceof BitmapDrawable) {
                        bitmap = BitmapFactory.decodeResource(context.getResources(), resID);
                    } else if (drawable instanceof VectorDrawable || drawable instanceof VectorDrawableCompat) {
                        int requireWidth = drawable.getIntrinsicWidth() <= 0 ? 1 : drawable.getIntrinsicWidth();
                        int requireHeight = drawable.getIntrinsicHeight() <= 0 ? 1 : drawable.getIntrinsicHeight();
                        Logger.d(TAG, "图片大小 requireWidth ：" + requireWidth + " ;requireHeight :" + requireHeight);
                        bitmap = createBitmapSafely(requireWidth, requireHeight, Bitmap.Config.ARGB_8888, CREATE_BITMAP_RETRY_COUNT);
                        DEF_CANVAS.get().setBitmap(bitmap);
                        DEF_CANVAS.get().save();
                        // 防止 View 上面有些区域空白导致最终 Bitmap 上有些区域变黑
                        DEF_CANVAS.get().drawColor(Color.TRANSPARENT);
                        drawable.setBounds(0, 0, requireWidth, requireHeight);
                        drawable.draw(DEF_CANVAS.get());
                        DEF_CANVAS.get().restore();
                        DEF_CANVAS.get().setBitmap(null);
                    }
                }

                if (null != rootView) {
                    Logger.e(TAG, "通过 布局 创建纹理； 如果存在同名布局和图片 会采用布局 ：" + markerId);
                    if (processor != null) {
                        Logger.d(TAG, "获取布局处理工具自定义布局样式 ：" + markerId);
                        if (isFocus) {
                            processor.onFocusProcess(rootView, data);
                        } else {
                            processor.onNormalProcess(rootView, data);
                        }
                    }
                    rootView.measure(0, 0);
                    rootView.layout(0, 0, rootView.getMeasuredWidth(), rootView.getMeasuredHeight());
                    int requireWidth = (rootView.getMeasuredWidth() <= 0 ? 1 : rootView.getMeasuredWidth());
                    int requireHeight = (rootView.getMeasuredHeight() <= 0 ? 1 : rootView.getMeasuredHeight());
                    bitmap = createBitmapSafely(requireWidth, requireHeight, Bitmap.Config.ARGB_8888, CREATE_BITMAP_RETRY_COUNT);
                    DEF_CANVAS.get().setBitmap(bitmap);
                    DEF_CANVAS.get().save();
                    // 防止 View 上面有些区域空白导致最终 Bitmap 上有些区域变黑
                    DEF_CANVAS.get().drawColor(Color.TRANSPARENT);
                    rootView.draw(DEF_CANVAS.get());
                    DEF_CANVAS.get().restore();
                    DEF_CANVAS.get().setBitmap(null);
                }
                if (null == bitmap) {
                    Logger.e(TAG, "创建bitmap失败. markerId: " + markerId);
                }
                return bitmap;
            }
        }
    }


    /**
     * 安全的创建bitmap;如果新建 Bitmap 时产生了 OOM，可以主动进行一次 GC - System.gc()，然后再次尝试创建
     *
     * @return bitmap
     */
    private Bitmap createBitmapSafely(int width, int height, Bitmap.Config config, int retryCount) {
        try {
            return Bitmap.createBitmap(width, height, config);
        } catch (OutOfMemoryError e) {
            if (retryCount > 0) {
                System.gc();
                Logger.e(TAG, "创建布局纹理产生 OOM");
                return createBitmapSafely(width, height, config, retryCount - 1);
            }
            return null;
        }
    }

    /**
     * 构建3D模型.
     *
     * @param str3DModelId
     * @return 建模对象
     */
    public Layer3DModel createLayer3DModel(String str3DModelId) {
        byte[] buffer = FileUtils.getInstance().getAssetFileContent(str3DModelId);
        Layer3DModel modelParam = new Layer3DModel();
        modelParam.resourceID = createMarkerId();
        modelParam.dataBuff = new BinaryStream(buffer);
        Logger.d(TAG, "创建3d模型 {：" + str3DModelId + "; resourceID : " + modelParam.resourceID + "}");
        return modelParam;
    }


    public LayerTexture getArrowRoadImage(boolean isArrow, CrossImageEntity crossInfo) {
        int RES_ID = 888888888;
        LayerTexture image = new LayerTexture();
        image.dataBuff = new BinaryStream(isArrow ? crossInfo.getArrowDataBuf() : crossInfo.getDataBuf());
        //栅格图箭头png
        image.iconType = isArrow ? LayerIconType.LayerIconTypePNG : LayerIconType.LayerIconTypeJPG;
        image.resID = RES_ID;
        image.isGenMipmaps = false;
        image.isPreMulAlpha = true;
        image.isRepeat = false;
        image.anchorType = LayerIconAnchor.LayerIconAnchorLeftTop;
        return image;
    }

}
