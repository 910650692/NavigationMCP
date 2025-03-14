package com.fy.navi.scene.impl.navi.common;


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
 */
public class NaviUiUtil {
    public static final String[] unitDistArray = new String[]{"m", "km", "米", "公里"};

    public static void showView(View view) {
        setViewVisibility(view, View.VISIBLE);
    }

    public static void hideView(View view) {
        setViewVisibility(view, View.GONE);
    }

    public static void hideView(View... viewList) {
        for (View view : viewList) {
            setViewVisibility(view, View.GONE);
        }
    }

    public static void invisibleView(View view) {
        setViewVisibility(view, View.INVISIBLE);
    }

    public static void setViewVisibility(View view, int visibility) {
        if (view != null) {
            view.setVisibility(visibility);
        }
    }

    public static Bitmap getRoadSignBitmap(byte[] roadSrc, int width, int height, int maneuverId, int roundNum, String hudResPrefix,
                                           String iconResName, String night) {
        return getRoadSignBitmap(roadSrc, width, height, maneuverId, roundNum, true, hudResPrefix, iconResName, night);
    }

    /**
     * 无在线数据（roadSrc为空）及isHud为true 时获取本地图片资源
     */
    public static Bitmap getRoadSignBitmap(byte[] roadSrc, int width, int height, int maneuverId, int roundNum, boolean isHud,
                                           String hudResPrefix, String iconResName, String night) {

        Bitmap roadSignBmp;
        int imageResId;
        if (isHud) {
            iconResName = hudResPrefix + iconResName;
        }

        if (roundNum > 0) {
            /*11进入环岛图标，右侧通行地区的逆时针环岛,12驶出环岛图标，右侧通行地区的逆时针环岛*/
            if (maneuverId == 12 || maneuverId == 11) {
                imageResId = getDrawableID(iconResName, String.valueOf(49 + roundNum));
                roadSignBmp = BitmapFactory.decodeResource(Resources.getSystem(), imageResId);
//                Logger.d("getRoadSignBitmap", " 1 maneuverId :" + maneuverId);
                return roadSignBmp;
            } else if (maneuverId == 18 || maneuverId == 17) {
                /*17进入环岛图标，左侧通行地区的顺时针环岛,18驶出环岛图标，左侧通行地区的顺时针环岛*/
                imageResId = getDrawableID(iconResName, String.valueOf(59 + roundNum));
                roadSignBmp = BitmapFactory.decodeResource(Resources.getSystem(), imageResId);
//                Logger.d("getRoadSignBitmap", " 2 maneuverId :" + maneuverId);
                return roadSignBmp;
            }
        }
        if (maneuverId == 65) {
            //1076B新增，65靠左图标
            imageResId = getDrawableID(iconResName, String.valueOf(6 + maneuverId));
            roadSignBmp = BitmapFactory.decodeResource(Resources.getSystem(), imageResId);
//            Logger.d("getRoadSignBitmap", " 3 maneuverId :" + maneuverId);
            return roadSignBmp;
        } else if (maneuverId == 66) {
            //1076B新增，66靠右图标
            imageResId = getDrawableID(iconResName, String.valueOf(4 + maneuverId));
            roadSignBmp = BitmapFactory.decodeResource(Resources.getSystem(), imageResId);
//            Logger.d("getRoadSignBitmap", " 4 maneuverId :" + maneuverId);
            return roadSignBmp;
        }
        if (roadSrc != null && roadSrc.length > 0) {
            roadSignBmp = bytesToBimap(roadSrc);
//            Logger.d("getRoadSignBitmap", " 5 maneuverId :" + maneuverId);
        } else {
            imageResId = getDrawableID(iconResName, maneuverId + "");
            roadSignBmp = BitmapFactory.decodeResource(Resources.getSystem(), imageResId);
//            Logger.d("getRoadSignBitmap", " 6 maneuverId :" + maneuverId);
        }

        return roadSignBmp;
    }

    public static Bitmap bytesToBimap(byte[] b) {
        return BitmapFactory.decodeByteArray(b, 0, b.length);
    }

    @SuppressLint("PrivateApi")
    public static int getDrawableID(String path, String id) {
        Field f = null;
        int drawableId = 0;
        try {
            f = com.fy.navi.scene.R.drawable.class.getDeclaredField(path + id);
            drawableId = f.getInt(com.fy.navi.scene.R.drawable.class);
        } catch (Exception e) {
        }
        return drawableId;
    }

    /**
     * 获取离线导航转向图片id
     *
     * @param maneuverId
     * @param roundNum
     * @return
     */
    public static int GetOfflineManeuverIconId(int maneuverId, int roundNum) {
        int maneuverPicId = maneuverId;
        int maxIconOffset = 0x42 + 1; // 最好是bl枚举提供一个值，后面就不用再改了; 需要bl改接口，后续再对接一次。。

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
     */
    public static void setTextViewContent(TextView tv, String content) {
        if (tv == null || TextUtils.isEmpty(content)) {
            return;
        }
        tv.setText(content);
    }

    public static void setTextViewContent(TextView tv, int resId) {
        if (tv == null || resId == 0) {
            return;
        }
        tv.setText(resId);
    }

    public static int getDayDiff(Calendar day) {
        /**
         * 今天日期
         */
        Calendar today = Calendar.getInstance();
        today.setTimeInMillis(System.currentTimeMillis());
        int dayDiff = day.get(Calendar.DAY_OF_YEAR);
        if (day.get(Calendar.YEAR) != today.get(Calendar.YEAR)) {
            dayDiff += getDaysInYear(today.get(Calendar.YEAR));
        }
        int todayDiff = today.get(Calendar.DAY_OF_YEAR);

        return dayDiff - todayDiff;
    }

    public static int getDaysInYear(int years) {
        Calendar cal = Calendar.getInstance();
        cal.set(years, Calendar.DECEMBER, 31);
        return cal.get(Calendar.DAY_OF_YEAR);
    }

    public static int getDimension(Context context, int resId) {
        return (int) context.getResources().getDimension(resId);
    }

    public static String getString(Context context, int resId) {
        return context.getResources().getString(resId);
    }

    public static String getString(Context context, int resId, Object... formatArgs) {
        return context.getResources().getString(resId, formatArgs);
    }

    /**
     * 按照固定的策略取整距离数值，与TBT保持一致
     * 1）10公里级别向下取整；
     * 2）1公里级别的四舍五入；
     * 3）1公里以下的暂不修改。
     *
     * @param distance
     * @return
     */
    public static String[] formatDistanceArray(Context context, int distance) {
        String[] distancs = new String[2];
        if (distance >= 10000) {
            //10公里级
            distance = (distance / 1000) * 1000;
        } else if (distance >= 1000) {
            //1公里级，精确到小数点后一位
            distance = ((distance + 50) / 100) * 100;
        }

        if (distance >= 1000) {
            int kiloMeter = distance / 1000;
            int leftMeter = distance % 1000;
            leftMeter = leftMeter / 100;

            StringBuffer sb = new StringBuffer();

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

    @SuppressLint("NewApi")//引用处已经做了版本兼容
    public static void setTranslationY(View view, float translationY) {
        view.setTranslationY(translationY);
    }

    @SuppressLint("NewApi")//引用处已经做了版本兼容
    public static void setTranslation(View view, float translation, boolean isX) {
        if (isX) {
            view.setTranslationX(translation);
        } else {
            view.setTranslationY(translation);
        }
    }

    public static int dipToPixel(Context context, int dipValue) {
        if (context == null) {
            return dipValue; // 原值返回
        }
        try {
            float pixelFloat = TypedValue.applyDimension(
                    TypedValue.COMPLEX_UNIT_DIP, dipValue, context
                            .getResources().getDisplayMetrics());
            return (int) pixelFloat;
        } catch (Exception e) {

        }
        return dipValue;
    }

    /*** 获取基于屏幕的控件矩形区域位置 ***/
    public static AutoUIViewRect getAutoUIViewRect(View view) {
        // 获取控件的全局坐标
        int[] location = new int[2];
        view.getLocationOnScreen(location);

        // 获取应用窗口的可见显示区域
        Rect windowVisibleDisplayFrame = new Rect();
        view.getWindowVisibleDisplayFrame(windowVisibleDisplayFrame);

        // 计算控件相对于应用显示区域的坐标
        int relativeLeft = location[0] + windowVisibleDisplayFrame.left;
        int relativeTop = location[1] + windowVisibleDisplayFrame.top;

        // 获取控件的宽度和高度
        int width = view.getWidth();
        int height = view.getHeight();
        // 构建相对于应用显示区域的矩形
        Rect relativeRect = new Rect(relativeLeft, relativeTop, relativeLeft + width, relativeTop + height);
//        Logger.i("getLocationOnApplication location " + location[0] + "," + location[1]);
//        getGlobalVisibleRect(view, new Rect());
//        Logger.i("getLocationOnApplication windowVisibleDisplayFrame " + windowVisibleDisplayFrame);
//        Logger.i("getLocationOnApplication width " + width + ",height：" + height);
//        Logger.i("getLocationOnApplication relativeRect " + relativeRect);
        return new AutoUIViewRect(relativeRect, new Rect(), new Rect());
    }

    /***获取基于应用显示区域的控件矩形区域位置***/
    public static void getLocationOnApplication(View view, Rect outRect) {
        if (view == null) {
            return;
        }
        Rect windowDisplayFrame = new Rect();
        view.getWindowVisibleDisplayFrame(windowDisplayFrame);
        Rect globalVisibleRect = new Rect();
        view.getGlobalVisibleRect(globalVisibleRect);
//        Logger.i("getLocationOnApplication windowDisplayFrame " + windowDisplayFrame + ",globalVisibleRect " + globalVisibleRect);
        outRect.left = globalVisibleRect.left - windowDisplayFrame.left;
        outRect.top = globalVisibleRect.top - windowDisplayFrame.top;
        outRect.right = globalVisibleRect.right - windowDisplayFrame.left;
        outRect.bottom = globalVisibleRect.bottom - windowDisplayFrame.top;
    }

    /*** 获取基于屏幕的控件矩形区域位置**/
    public static void getGlobalVisibleRect(View view, Rect rect) {
        view.getGlobalVisibleRect(rect);
        Logger.e("getLocationOnScreen rectrectrectrect " + rect);// Rect(248, 199 - 904, 444)
    }

    /*** 获取基于父控件的矩形区域位置 **/
    public static void getLocationInParent(View view, Rect outRect) {
        outRect.left = view.getLeft();
        outRect.top = view.getTop();
        outRect.right = view.getRight();
        outRect.bottom = view.getBottom();
        Logger.e("getLocationOnApplication getLocationInParent " + outRect);
    }
}
