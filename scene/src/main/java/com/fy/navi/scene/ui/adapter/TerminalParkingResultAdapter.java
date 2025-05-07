
package com.fy.navi.scene.ui.adapter;


import android.text.Spannable;
import android.text.SpannableString;
import android.text.style.ForegroundColorSpan;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.TerminalParkingItemBinding;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TerminalParkingResultAdapter extends RecyclerView.Adapter<TerminalParkingResultAdapter.ResultHolder> {
    private static final Map<Integer, String> BUSY_STATUS_MAP = new HashMap<>();
    private final List<PoiInfoEntity> mPoiEntities;
    private OnItemClickListener mOnItemClickListener;
    private int mSelectedPosition = -1;
    public static final int IDLE = 1;
    public static final int ENOUGH = 2;
    public static final int LESS = 3;
    public static final int BUSY = 4;
    public static final int FULL = 5;

    static {
        BUSY_STATUS_MAP.put(IDLE, "充足");
        BUSY_STATUS_MAP.put(ENOUGH, "够用");
        BUSY_STATUS_MAP.put(LESS, "较少");
        BUSY_STATUS_MAP.put(BUSY, "紧张");
        BUSY_STATUS_MAP.put(FULL, "已满");
    }

    public void setOnItemClickListener(final OnItemClickListener listener) {
        mOnItemClickListener = listener;
    }

    public TerminalParkingResultAdapter() {
        this.mPoiEntities = new ArrayList<>();
    }

    /**
     * 更新列表
     * @param searchResultEntity searchResultEntity 源数据
     */
    public void notifyList(final SearchResultEntity searchResultEntity) {
        final List<PoiInfoEntity> newPoiList = searchResultEntity.getPoiList();
        final int oldSize = mPoiEntities.size();
        final int newSize = newPoiList.size();

        mPoiEntities.clear();
        mPoiEntities.addAll(newPoiList);

        if (oldSize == 0 && newSize > 0) {
            notifyItemRangeInserted(0, newSize);
        } else if (oldSize > 0 && newSize == 0) {
            notifyItemRangeRemoved(0, oldSize);
        } else if (oldSize > 0) {
            notifyItemRangeChanged(0, Math.min(oldSize, newSize));
            if (newSize > oldSize) {
                notifyItemRangeInserted(oldSize, newSize - oldSize);
            } else if (newSize < oldSize) {
                notifyItemRangeRemoved(newSize, oldSize - newSize);
            }
        }
    }

    @NonNull
    @Override
    public ResultHolder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final TerminalParkingItemBinding adapterSearchResultItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.terminal_parking_item, parent, false);
        return new ResultHolder(adapterSearchResultItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull final ResultHolder holder, final int position) {
        holder.itemView.setSelected(position == mSelectedPosition);

        // 获取当前 POI 信息
        final PoiInfoEntity poiEntity = mPoiEntities.get(position);
        final ParkingInfo parkingInfo = poiEntity.getParkingInfoList().get(0);

        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "position: " + poiEntity.getParkingInfoList().size());

        // 设置停车场基本信息
        holder.mTerminalParkingItemBinding.sktvParkingItemNum.setText(String.valueOf(position + 1));
        holder.mTerminalParkingItemBinding.sktvParkingName.setText(poiEntity.getName());


        final String totalSpace = String.valueOf(parkingInfo.getSpace());
        final String freeSpace = String.valueOf(parkingInfo.getSpaceFree());
        holder.mTerminalParkingItemBinding.sktvParkingItemLeisureNum.setText(getColoredParkingInfo(totalSpace, freeSpace));
        // 设置繁忙状态
        holder.mTerminalParkingItemBinding.sktvParkingItemSufficientNum.setText(getBusStatusString(position));

        // 当前位置显示
        holder.mTerminalParkingItemBinding.sktvParkingItemDistance.setText(
                AppContext.getInstance().getMContext().getString(R.string.st_distance_to_finish, poiEntity.getDistance()));

        // 处理 item 点击事件（选中状态更新）
        holder.itemView.setOnClickListener(v -> updateSelectedPosition(holder.getAdapterPosition()));

        // 处理导航按钮点击事件
        holder.mTerminalParkingItemBinding.poiToNavi.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click 算路|添加途经点");
            if (mOnItemClickListener != null) {
                mOnItemClickListener.onNaviClick(position, poiEntity);
            }
        });
    }

    /**
     * 生成不同颜色的停车位信息
     * @param totalSpace 总停车位
     * @param freeSpace 剩余停车位
     * @return SpannableString 停车位信息
     */
    private SpannableString getColoredParkingInfo(final String totalSpace, final String freeSpace) {
        final String text = totalSpace + " / " + freeSpace;
        final SpannableString spannableString = new SpannableString(text);
        final int colorDark = ContextCompat.getColor(AppContext.getInstance().getMContext(), R.color.poi_details_bottom_ff_00);
        final int colorLight = ContextCompat.getColor(AppContext.getInstance().getMContext(), R.color.search_quick_tab_view_color);
        spannableString.setSpan(new ForegroundColorSpan(colorDark), 0, totalSpace.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
        spannableString.setSpan(new ForegroundColorSpan(colorLight), text.length() - freeSpace.length(),
                text.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
        return spannableString;
    }

    /**
     * 更新选中位置，并刷新 UI
     * @param newPosition 新位置
     */
    private void updateSelectedPosition(final int newPosition) {
        if (mSelectedPosition >= 0) {
            notifyItemChanged(mSelectedPosition);
        }
        mSelectedPosition = newPosition;
        notifyItemChanged(mSelectedPosition);
    }

    /**
     * 获取停车场繁忙状态
     * @param position 选中下标
     * @return 繁忙状态
     */
    private String getBusStatusString(final int position) {
        final int busyStatus = mPoiEntities.get(position).getParkingInfoList().get(0).getBusyStatus();
        return BUSY_STATUS_MAP.getOrDefault(busyStatus, "未知");
    }

    @Override
    public int getItemCount() {
        return mPoiEntities.size();
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        private final TerminalParkingItemBinding mTerminalParkingItemBinding;

        public ResultHolder(final TerminalParkingItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.mTerminalParkingItemBinding = resultItemBinding;
            this.mTerminalParkingItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        /**
         * item 点击事件
         * @param position 点击下标
         * @param poiInfoEntity 点击 POI 信息
         */
        void onItemClick(int position, PoiInfoEntity poiInfoEntity);

        /**
         * 导航点击事件
         * @param position 点击下标
         * @param poiInfoEntity 点击 POI 信息
         */
        void onNaviClick(int position, PoiInfoEntity poiInfoEntity);
    }
}