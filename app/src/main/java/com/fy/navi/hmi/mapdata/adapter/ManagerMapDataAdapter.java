package com.fy.navi.hmi.mapdata.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;

import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.utils.StringUtils;
import com.fy.navi.scene.ui.setting.DownloadBtnView;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.view.CustomSwipeMenuLayout;
import com.fy.navi.ui.view.SkinConstraintLayout;
import com.fy.navi.ui.view.SkinImageView;
import com.fy.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.List;

public class ManagerMapDataAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {
    private static final int TYPE_PARENT = 0;
    private static final int TYPE_CHILD = 1;
    private Context mContext;
    private List<ProvDataInfo> parentItems = new ArrayList<>();
    private OnChildClickListener onChildClickListener;

    public ManagerMapDataAdapter(final Context context) {
        this.mContext = context;
    }

    /**
     * 设置数据
     * @param parentItems
     */
    public void setData(List<ProvDataInfo> parentItems) {
        this.parentItems = parentItems;
        notifyDataSetChanged();
    }

    public void setOnChildClickListener(final OnChildClickListener listener) {
        this.onChildClickListener = listener;
    }

    @Override
    public int getItemViewType(int position) {
        int parentIndex = 0;
        int itemCount = 0;

        for (ProvDataInfo parent : parentItems) {
            if (position == itemCount) {
                return TYPE_PARENT;
            }
            itemCount++;

            if (parent.isExpanded()) {
                if (position < itemCount + parent.getCityInfoList().size()) {
                    return TYPE_CHILD;
                }
                itemCount += parent.getCityInfoList().size();
            }
        }
        throw new IllegalStateException("Invalid position");
    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        LayoutInflater inflater = LayoutInflater.from(parent.getContext());
        if (viewType == TYPE_PARENT) {
            View view = inflater.inflate(R.layout.item_manager_province, parent, false);
            return new ParentViewHolder(view);
        } else {
            View view = inflater.inflate(R.layout.item_city_data, parent, false);
            return new ChildViewHolder(view);
        }
    }

    @Override
    public void onBindViewHolder(RecyclerView.ViewHolder holder, int position) {
        int parentIndex = 0;
        int itemCount = 0;

        for (ProvDataInfo parent : parentItems) {
            if (position == itemCount) {
                ((ParentViewHolder) holder).bind(parent);
                return;
            }
            itemCount++;

            if (parent.isExpanded()) {
                if (position < itemCount + parent.getCityInfoList().size()) {
                    int childPos = position - itemCount;
                    ((ChildViewHolder) holder).bind(parent, parent.getCityInfoList().get(childPos));
                    return;
                }
                itemCount += parent.getCityInfoList().size();
            }
        }
        throw new IllegalStateException("Invalid position");
    }

    @Override
    public int getItemCount() {
        if (parentItems != null && !parentItems.isEmpty()) {
            int count = parentItems.size();
            for (ProvDataInfo parent : parentItems) {
                if (parent.isExpanded()) {
                    count += parent.getCityInfoList().size();
                }
            }
            return count;
        } else {
            return 0;
        }
    }

    /**
     * 父项ViewHolder
     */
    class ParentViewHolder extends RecyclerView.ViewHolder {
        private SkinTextView title;

        ParentViewHolder(View itemView) {
            super(itemView);
            title = itemView.findViewById(R.id.item_manager_province_name);
        }

        void bind(ProvDataInfo parent) {
            title.setText(parent.getName()); //省份名称
            if (parent.getName().equals("基础功能包")) {
                title.setVisibility(View.GONE);
            } else {
                title.setVisibility(View.VISIBLE);
            }
        }
    }

    /**
     * 子项ViewHolder
     */
    class ChildViewHolder extends RecyclerView.ViewHolder {
        private SkinTextView cityName;
        private SkinTextView cityData;
        private DownloadBtnView mDownloadBtnView;
        private CustomSwipeMenuLayout swipeMenuLayout;
        private SkinTextView deleteCity;
        private SkinTextView cityNewLabel;
        private ProgressBar mDownloadProgress;

        ChildViewHolder(View itemView) {
            super(itemView);
            cityName = itemView.findViewById(R.id.item_city_name);
            cityData = itemView.findViewById(R.id.item_city_data);
            swipeMenuLayout = itemView.findViewById(R.id.swipe_menu_layout);
            mDownloadBtnView = itemView.findViewById(R.id.item_download_status);
            deleteCity = itemView.findViewById(R.id.item_driving_delete);
            cityNewLabel = itemView.findViewById(R.id.item_city_new_label);
            mDownloadProgress = itemView.findViewById(R.id.download_progress);
        }

        void bind(ProvDataInfo parent, CityDataInfo child) {
            final CityDownLoadInfo downloadItem = child.getDownLoadInfo();
            // 非已下载状态，禁止侧滑删除
            if (downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                swipeMenuLayout.setSwipeEnabled(true);
            } else {
                swipeMenuLayout.setSwipeEnabled(false);
                swipeMenuLayout.smoothClose();
            }
            //是否显示“新”标签
            if (child.isNew()) {
                cityNewLabel.setVisibility(View.VISIBLE);
            } else {
                cityNewLabel.setVisibility(View.GONE);
            }

            // 城市名称
            cityName.setText(child.getName());
            // 城市数据包大小
            final String sizeString = StringUtils.formatSize(downloadItem.getFullZipSize().longValue());
            cityData.setText(sizeString);
            // 下载按钮状态
            mDownloadBtnView.parseDownloadStatusInfo(child.getDownLoadInfo());
            final boolean isShowDownloadProgress = child.getDownLoadInfo().getTaskState()  == UserDataCode.TASK_STATUS_CODE_DOING
                    || child.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_DONE
                    || child.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_UNZIPPING
                    || child.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_PAUSE;
            if (isShowDownloadProgress) {
                mDownloadProgress.setProgress((int) Math.floor(child.getDownLoadInfo().getPercent()));
                mDownloadProgress.setVisibility(View.VISIBLE);
            } else {
                mDownloadProgress.setVisibility(View.GONE);
            }

            // 下载 or 暂停下载
            mDownloadBtnView.setOnClickListener(v -> {
                Logger.d( "parent: " + GsonUtils.toJson(parent) + " child: " + GsonUtils.toJson(child));
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                cityAdCodes.add(downloadItem.getAdcode());
                if (onChildClickListener != null) {
                    switch (downloadItem.getTaskState()) {
                        case UserDataCode.TASK_STATUS_CODE_DOING:  // 下载中 or 更新中（downloadItem.bIsDataUsed = true 更新中）
                        case UserDataCode.TASK_STATUS_CODE_DONE:   // 下载中 or 更新中（downloadItem.bIsDataUsed = true 更新中）
                        case UserDataCode.TASK_STATUS_CODE_WAITING: // 等待中 or 等待更新中
                            onChildClickListener.pauseAllTask(cityAdCodes);
                            break;
                        case UserDataCode.TASK_STATUS_CODE_PAUSE:  // 暂停
                        case UserDataCode.TASK_STATUS_CODE_READY:  // 待下载 or 待更新 (downloadItem.bIsDataUsed = true 待更新)
                        case UserDataCode.TASK_STATUS_CODE_ERR: // 错误 - 重试
                        case UserDataCode.TASK_STATUS_CODE_MAX: // 重试
                            onChildClickListener.startAllTask(cityAdCodes);
                            break;
                        default:
                            break;
                    }
                }
            });
            // 删除已下载数据
            deleteCity.setOnClickListener(view -> {
                Logger.d( "parent: " + GsonUtils.toJson(parent) + " child: " + GsonUtils.toJson(child));
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                cityAdCodes.add(downloadItem.getAdcode());
                if (onChildClickListener != null) {
                    onChildClickListener.deleteAllTask(cityAdCodes);
                }
            });
        }
    }

    /**
     * 更新子项数据
     * @param parentId
     * @param childId
     * @param newValue
     */
    public void updateChild(int parentId, int childId, CityDownLoadInfo newValue) {
        int childPosition = getChildPosition(parentId, childId);
        for (ProvDataInfo parent : parentItems) {
            if (parent.getAdcode() == parentId) {
                for (CityDataInfo child : parent.getCityInfoList()) {
                    if (child.getAdcode() == childId) {
                        child.setDownLoadInfo(newValue);
                        notifyItemChanged(childPosition);
                        return;
                    }
                }
            }
        }
    }

    /**
     * 辅助方法：根据位置获取父项索引
     * @param adapterPosition
     * @return
     */
    private int getParentPosition(int adapterPosition) {
        int parentIndex = 0;
        int itemCount = 0;

        for (ProvDataInfo parent : parentItems) {
            if (adapterPosition == itemCount) {
                return parentIndex;
            }
            itemCount++;

            if (parent.isExpanded()) {
                if (adapterPosition < itemCount + parent.getCityInfoList().size()) {
                    return parentIndex;
                }
                itemCount += parent.getCityInfoList().size();
            }
            parentIndex++;
        }
        throw new IllegalStateException("Invalid position");
    }

    /**
     * 新增方法：根据父项和子项ID获取子项的绝对位置
     * @param parentId
     * @param childId
     * @return
     */
    public int getChildPosition(int parentId, int childId) {
        int position = 0;

        for (ProvDataInfo parent : parentItems) {
            position++; // 父项位置

            if (parent.getAdcode() == parentId) {
                if (parent.isExpanded()) {
                    for (CityDataInfo child : parent.getCityInfoList()) {
                        if (child.getAdcode() == childId) {
                            return position;
                        }
                        position++;
                    }
                }
                break;
            }

            if (parent.isExpanded()) {
                position += parent.getCityInfoList().size();
            }
        }
        return -1; // 未找到
    }

    public interface OnChildClickListener {
        /**
         * 开始下载
         * @param cityAdCodes
         */
        void startAllTask(final ArrayList<Integer> cityAdCodes);
        /**
         * 暂停下载
         * @param cityAdCodes
         */
        void pauseAllTask(final ArrayList<Integer> cityAdCodes);
        /**
         * 删除
         * @param cityAdCodes
         */
        void deleteAllTask(final ArrayList<Integer> cityAdCodes);

    }

}