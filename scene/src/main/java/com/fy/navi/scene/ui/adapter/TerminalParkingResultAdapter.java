
package com.fy.navi.scene.ui.adapter;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.text.Spannable;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
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
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TerminalParkingResultAdapter extends RecyclerView.Adapter<TerminalParkingResultAdapter.ResultHolder> {
    private static final Map<Integer, String> BUSY_STATUS_MAP = new HashMap<>();
    private final List<PoiInfoEntity> poiEntities;
    private OnItemClickListener onItemClickListener;
    private int selectedPosition = -1;
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

    public void setOnItemClickListener(OnItemClickListener listener) {
        onItemClickListener = listener;
    }

    public TerminalParkingResultAdapter() {
        this.poiEntities = new ArrayList<>();
    }

    public void notifyList(SearchResultEntity searchResultEntity) {
        List<PoiInfoEntity> newPoiList = searchResultEntity.getPoiList();
        int oldSize = poiEntities.size();
        int newSize = newPoiList.size();

        poiEntities.clear();
        poiEntities.addAll(newPoiList);

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
    public ResultHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        TerminalParkingItemBinding adapterSearchResultItemBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.terminal_parking_item, parent, false);
        return new ResultHolder(adapterSearchResultItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull ResultHolder holder, int position) {
        holder.itemView.setSelected(position == selectedPosition);

        // 获取当前 POI 信息
        PoiInfoEntity poiEntity = poiEntities.get(position);
        ParkingInfo parkingInfo = poiEntity.getParkingInfoList().get(0);

        Logger.d(SEARCH_HMI_TAG, "position: " + poiEntity.getParkingInfoList().size());

        // 设置停车场基本信息
        holder.terminalParkingItemBinding.sktvParkingItemNum.setText(String.valueOf(position + 1));
        holder.terminalParkingItemBinding.sktvParkingName.setText(poiEntity.getName());


        String totalSpace = String.valueOf(parkingInfo.getSpace());
        String freeSpace = String.valueOf(parkingInfo.getSpaceFree());
        holder.terminalParkingItemBinding.sktvParkingItemLeisureNum.setText(getColoredParkingInfo(totalSpace, freeSpace));
        // 设置繁忙状态
        holder.terminalParkingItemBinding.sktvParkingItemSufficientNum.setText(getBusStatusString(position));

        // 当前位置显示
        holder.terminalParkingItemBinding.sktvParkingItemDistance.setText(AppContext.getInstance().getMContext().getString(R.string.st_distance_to_finish, poiEntity.getDistance()));

        // 处理 item 点击事件（选中状态更新）
        holder.itemView.setOnClickListener(v -> updateSelectedPosition(holder.getAdapterPosition()));

        // 处理导航按钮点击事件
        holder.terminalParkingItemBinding.poiToNavi.setOnClickListener(v -> {
            Logger.d(SEARCH_HMI_TAG, "poi click 算路|添加途经点");
            if (onItemClickListener != null) {
                onItemClickListener.onNaviClick(position, poiEntity);
            }
        });
    }

    /**
     * 生成不同颜色的停车位信息
     */
    private SpannableString getColoredParkingInfo(String totalSpace, String freeSpace) {
        String text = totalSpace + " / " + freeSpace;
        SpannableString spannableString = new SpannableString(text);
        int colorDark = ContextCompat.getColor(AppContext.getInstance().getMContext(), R.color.black);
        int colorLight = ContextCompat.getColor(AppContext.getInstance().getMContext(), R.color.search_loading_bg_80);
        spannableString.setSpan(new ForegroundColorSpan(colorDark), 0, totalSpace.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
        spannableString.setSpan(new ForegroundColorSpan(colorLight), text.length() - freeSpace.length(), text.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
        return spannableString;
    }

    /**
     * 更新选中位置，并刷新 UI
     */
    private void updateSelectedPosition(int newPosition) {
        if (selectedPosition >= 0) {
            notifyItemChanged(selectedPosition);
        }
        selectedPosition = newPosition;
        notifyItemChanged(selectedPosition);
    }

    private String getBusStatusString(int position) {
        int busyStatus = poiEntities.get(position).getParkingInfoList().get(0).getBusyStatus();
        return BUSY_STATUS_MAP.getOrDefault(busyStatus, "未知");
    }

    @Override
    public int getItemCount() {
        return poiEntities.size();
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        public TerminalParkingItemBinding terminalParkingItemBinding;

        public ResultHolder(TerminalParkingItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.terminalParkingItemBinding = resultItemBinding;
            this.terminalParkingItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        void onItemClick(int position, PoiInfoEntity poiInfoEntity);

        void onNaviClick(int position, PoiInfoEntity poiInfoEntity);
    }
}