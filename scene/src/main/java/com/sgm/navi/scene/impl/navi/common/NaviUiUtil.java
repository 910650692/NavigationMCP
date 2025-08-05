package com.sgm.navi.scene.impl.navi.common;


import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Rect;
import android.text.TextUtils;
import android.util.TypedValue;
import android.view.View;
import android.widget.TextView;


import com.android.utils.R;
import com.android.utils.log.Logger;

import java.lang.reflect.Field;
import java.util.Calendar;

/**
 * 导航UI控件Utils
 * @author sgm
 * @version $Revision.*$
 */
public final class NaviUiUtil {
    public static final String TAG = "NaviUiUtil";
    private NaviUiUtil() {

    }
    public static final String[] UNIT_DIST_ARRAY = new String[]{"m", "km", "米", "公里"};

    /**
     * 显示View
     * @param view the view to be shown
     */
    public static void showView(final View view) {
        setViewVisibility(view, View.VISIBLE);
    }

    /**
     * 隐藏View
     * @param view the view to be hidden
     */
    public static void hideView(final View view) {
        setViewVisibility(view, View.GONE);
    }

    /**
     * 隐藏View
     * @param viewList the views to be hidden
     */
    public static void hideView(final View... viewList) {
        for (View view : viewList) {
            setViewVisibility(view, View.GONE);
        }
    }

    /**
     * 设置View为不可见
     * @param view the view to be invisible
     */
    public static void invisibleView(final View view) {
        setViewVisibility(view, View.INVISIBLE);
    }

    /**
     * 设置View的可见性
     * @param view the view to be set
     * @param visibility the visibility to be set
     */
    public static void setViewVisibility(final View view, final int visibility) {
        if (view != null) {
            view.setVisibility(visibility);
        }
    }

    /**
     * 获取路标位图
     * @param roadSrc roadSrc
     * @param width width
     * @param height height
     * @param maneuverId maneuverId
     * @param roundNum roundNum
     * @param hudResPrefix hudResPrefix
     * @param iconResName iconResName
     * @param night night
     * @return Bitmap
     */
    public static Bitmap getRoadSignBitmap(final byte[] roadSrc, final int width, final int height,
                                           final int maneuverId, final int roundNum,
                                           final String hudResPrefix,
                                           final String iconResName, final String night) {
        return getRoadSignBitmap(roadSrc, width, height, maneuverId, roundNum, true,
                hudResPrefix, iconResName, night);
    }

    /**
     * 无在线数据（roadSrc为空）及isHud为true 时获取本地图片资源
     * @param roadSrc roadSrc
     * @param width width
     * @param height height
     * @param maneuverId maneuverId
     * @param roundNum roundNum
     * @param isHud isHud
     * @param hudResPrefix hudResPrefix
     * @param iconResName iconResName
     * @param night night
     * @return Bitmap
     */
    public static Bitmap getRoadSignBitmap(final byte[] roadSrc, final int width, final int height,
                                           final int maneuverId, final int roundNum,
                                           final boolean isHud, final String hudResPrefix,
                                           String iconResName, final String night) {

        Bitmap roadSignBmp = null;
        int imageResId;
        if (isHud) {
            iconResName = hudResPrefix + iconResName;
        }

        if (roundNum > 0) {
            /*11进入环岛图标，右侧通行地区的逆时针环岛,12驶出环岛图标，右侧通行地区的逆时针环岛*/
            if (maneuverId == 12 || maneuverId == 11) {
                imageResId = getDrawableID(iconResName, String.valueOf(49 + roundNum));
                if (imageResId != 0) {
                    roadSignBmp = BitmapFactory.decodeResource(Resources.getSystem(), imageResId);
                }
                return roadSignBmp;
            } else if (maneuverId == 18 || maneuverId == 17) {
                /*17进入环岛图标，左侧通行地区的顺时针环岛,18驶出环岛图标，左侧通行地区的顺时针环岛*/
                imageResId = getDrawableID(iconResName, String.valueOf(59 + roundNum));
                if (imageResId != 0) {
                    roadSignBmp = BitmapFactory.decodeResource(Resources.getSystem(), imageResId);
                }
                return roadSignBmp;
            }
        }
        if (maneuverId == 65) {
            //1076B新增，65靠左图标
            imageResId = getDrawableID(iconResName, String.valueOf(6 + maneuverId));
            if (imageResId != 0) {
                roadSignBmp = BitmapFactory.decodeResource(Resources.getSystem(), imageResId);
            }
            return roadSignBmp;
        } else if (maneuverId == 66) {
            //1076B新增，66靠右图标
            imageResId = getDrawableID(iconResName, String.valueOf(4 + maneuverId));
            if (imageResId != 0) {
                roadSignBmp = BitmapFactory.decodeResource(Resources.getSystem(), imageResId);
            }
            return roadSignBmp;
        }
        if (roadSrc != null && roadSrc.length > 0) {
            roadSignBmp = bytesToBimap(roadSrc);
        } else {
            imageResId = getDrawableID(iconResName, maneuverId + "");
            if (imageResId != 0) {
                roadSignBmp = BitmapFactory.decodeResource(Resources.getSystem(), imageResId);
            }
        }

        return roadSignBmp;
    }

    /**
     * @param b b
     * @return Bitmap
     */
    public static Bitmap bytesToBimap(final byte[] b) {
        return BitmapFactory.decodeByteArray(b, 0, b.length);
    }

    /**
     * 获取drawable的id
     * @param path path
     * @param id id
     * @return int
     */
    @SuppressLint("PrivateApi")
    public static int getDrawableID(final String path, final String id) {
        Field f = null;
        int drawableId = 0;
        try {
            f = com.sgm.navi.scene.R.drawable.class.getDeclaredField(path + id);
            drawableId = f.getInt(com.sgm.navi.scene.R.drawable.class);
        } catch (Exception e) {
            Logger.e(TAG, "getDrawableID error = ", e.getMessage());
        }
        return drawableId;
    }

    /**
     * 获取离线导航转向图片id
     *
     * @param maneuverId maneuverId
     * @param roundNum roundNum
     * @return int
     */
    public static int getOfflineManeuverIconId(final int maneuverId, final int roundNum) {
        int maneuverPicId = maneuverId;
        final int maxIconOffset = 0x42 + 1; // 最好是bl枚举提供一个值，后面就不用再改了; 需要bl改接口，后续再对接一次。。

        if (roundNum > 0) {
            if (maneuverId == 12 || maneuverId == 11) {
                /*  11是右行驶进入环岛，12是右行驶出环岛，需根据出口贴图片  */
                maneuverPicId = maxIconOffset + roundNum;
            } else if (maneuverId == 18 || maneuverId == 17) {
                /*  17是左行驶进入环岛，18是左行驶出环岛,需根据出口贴图片, roundNum最大定义了10份图片  */
                maneuverPicId = maxIconOffset + 11 + roundNum;
            }
        }
        return maneuverPicId;
    }

    /**
     * 设置TextView显示内容
     * @param tv tv
     * @param content content
     */
    public static void setTextViewContent(final TextView tv, final String content) {
        if (tv == null || TextUtils.isEmpty(content)) {
            return;
        }
        tv.setText(content);
    }

    /**
     * 设置TextView显示内容
     * @param tv tv
     * @param resId resId
     */
    public static void setTextViewContent(final TextView tv, final int resId) {
        if (tv == null || resId == 0) {
            return;
        }
        tv.setText(resId);
    }

    /**
     * 获取两个日期之间的天数差
     * @param day day
     * @return int
     */
    public static int getDayDiff(final Calendar day) {
        /**
         * 今天日期
         */
        final Calendar today = Calendar.getInstance();
        today.setTimeInMillis(System.currentTimeMillis());
        int dayDiff = day.get(Calendar.DAY_OF_YEAR);
        if (day.get(Calendar.YEAR) != today.get(Calendar.YEAR)) {
            dayDiff += getDaysInYear(today.get(Calendar.YEAR));
        }
        final int todayDiff = today.get(Calendar.DAY_OF_YEAR);

        return dayDiff - todayDiff;
    }

    /**
     * @param years years
     * @return 一年中的第几天
     */
    public static int getDaysInYear(final int years) {
        final Calendar cal = Calendar.getInstance();
        cal.set(years, Calendar.DECEMBER, 31);
        return cal.get(Calendar.DAY_OF_YEAR);
    }

    /**
     * @param context 上下文
     * @param resId 资源id
     * @return int
     */
    public static int getDimension(final Context context, final int resId) {
        return (int) context.getResources().getDimension(resId);
    }

    /**
     * @param context 上下文
     * @param resId 资源id
     * @return String
     */
    public static String getString(final Context context, final int resId) {
        return context.getResources().getString(resId);
    }

    /**
     * @param context 上下文
     * @param resId 资源id
     * @param formatArgs 格式化参数
     * @return String
     */
    public static String getString(final Context context, final int resId,
                                   final Object... formatArgs) {
        return context.getResources().getString(resId, formatArgs);
    }

    /**
     * 按照固定的策略取整距离数值，与TBT保持一致
     * 1）10公里级别向下取整；
     * 2）1公里级别的四舍五入；
     * 3）1公里以下的暂不修改。
     *
     * @param context  上下文
     * @param distance 距离
     * @return String[]
     */
    public static String[] formatDistanceArray(final Context context, int distance) {
        final String[] distancs = new String[2];
        if (distance >= 10000) {
            //10公里级
            distance = (distance / 1000) * 1000;
        } else if (distance >= 1000) {
            //1公里级，精确到小数点后一位
            distance = ((distance + 50) / 100) * 100;
        }

        if (distance >= 1000) {
            final int kiloMeter = distance / 1000;
            int leftMeter = distance % 1000;
            leftMeter = leftMeter / 100;

            final StringBuffer sb = new StringBuffer();

            if (leftMeter > 0) {
                sb.append(kiloMeter);
                sb.append(".");
                sb.append(leftMeter);
            } else {
                sb.append(kiloMeter);
            }
            distancs[0] = sb.toString();
            distancs[1] = getString(context, R.string.km);
        } else {
            distancs[0] = String.valueOf(distance);
            distancs[1] = getString(context, R.string.meter);
        }
        return distancs;
    }

    /**
     * 设置View的translationY
     * @param view view
     * @param translationY translationY
     */
    @SuppressLint("NewApi")//引用处已经做了版本兼容
    public static void setTranslationY(final View view, final float translationY) {
        view.setTranslationY(translationY);
    }

    /**
     * 设置View的translation
     * @param view view
     * @param translation translation
     * @param isX isX
     */
    @SuppressLint("NewApi")//引用处已经做了版本兼容
    public static void setTranslation(final View view, final float translation, final boolean isX) {
        if (isX) {
            view.setLayerType(View.LAYER_TYPE_SOFTWARE, null);
            view.setWillNotDraw(false);
            view.animate().translationX(translation).setDuration(0).start();
            view.setLayerType(View.LAYER_TYPE_HARDWARE, null);
        } else {
            view.setTranslationY(translation);
        }
    }

    /**
     * dip转换为px
     * @param context context
     * @param dipValue dipValue
     * @return int
     */
    public static int dipToPixel(final Context context, final int dipValue) {
        if (context == null) {
            return dipValue; // 原值返回
        }
        try {
            final float pixelFloat = TypedValue.applyDimension(
                    TypedValue.COMPLEX_UNIT_DIP, dipValue, context
                            .getResources().getDisplayMetrics());
            return (int) pixelFloat;
        } catch (Exception e) {
            Logger.e(TAG, "dipToPixel error ", e.getMessage());
        }
        return dipValue;
    }

    /**
     * 获取基于屏幕的控件矩形区域位置
     * @param view view
     * @return AutoUIViewRect
     **/
    public static AutoUIViewRect getAutoUIViewRect(final View view) {
        // 获取控件的全局坐标
        final int[] location = new int[2];
        view.getLocationOnScreen(location);

        // 获取应用窗口的可见显示区域
        final Rect windowVisibleDisplayFrame = new Rect();
        view.getWindowVisibleDisplayFrame(windowVisibleDisplayFrame);

        // 计算控件相对于应用显示区域的坐标
        final int relativeLeft = location[0] + windowVisibleDisplayFrame.left;
        final int relativeTop = location[1];

        // 获取控件的宽度和高度
        final int width = view.getWidth();
        final int height = view.getHeight();
        // 构建相对于应用显示区域的矩形
        final Rect relativeRect = new Rect(relativeLeft, relativeTop, relativeLeft + width,
                relativeTop + height);
//        Logger.i("getLocationOnApplication location " + location[0] + "," + location[1]);
//        getGlobalVisibleRect(view, new Rect());
//        Logger.i("getLocationOnApplication windowVisibleDisplayFrame " + windowVisibleDisplayFrame);
//        Logger.i("getLocationOnApplication width " + width + ",height：" + height);
//        Logger.i("getLocationOnApplication relativeRect " + relativeRect);
        return new AutoUIViewRect(relativeRect, new Rect(), new Rect());
    }

    /**
     * 获取基于应用显示区域的控件矩形区域位置
     * @param view view
     * @param outRect outRect
     **/
    public static void getLocationOnApplication(final View view, final Rect outRect) {
        if (view == null) {
            return;
        }
        final Rect windowDisplayFrame = new Rect();
        view.getWindowVisibleDisplayFrame(windowDisplayFrame);
        final Rect globalVisibleRect = new Rect();
        view.getGlobalVisibleRect(globalVisibleRect);
//        Logger.i("getLocationOnApplication windowDisplayFrame " + windowDisplayFrame + ",globalVisibleRect " + globalVisibleRect);
        outRect.left = globalVisibleRect.left - windowDisplayFrame.left;
        outRect.top = globalVisibleRect.top - windowDisplayFrame.top;
        outRect.right = globalVisibleRect.right - windowDisplayFrame.left;
        outRect.bottom = globalVisibleRect.bottom - windowDisplayFrame.top;
    }

    /**
     * 获取基于屏幕的控件矩形区域位置
     * @param view view
     * @param rect rect
     **/
    public static void getGlobalVisibleRect(final View view, final Rect rect) {
        view.getGlobalVisibleRect(rect);
        Logger.e("getLocationOnScreen rectrectrectrect " + rect);// Rect(248, 199 - 904, 444)
    }

    /**
     * 获取基于父控件的矩形区域位置
     * @param view view
     * @param outRect outRect
     **/
    public static void getLocationInParent(final View view, final Rect outRect) {
        outRect.left = view.getLeft();
        outRect.top = view.getTop();
        outRect.right = view.getRight();
        outRect.bottom = view.getBottom();
        Logger.e("getLocationOnApplication getLocationInParent " + outRect);
    }
}
